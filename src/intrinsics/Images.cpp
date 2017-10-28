/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Images.h"
#include "../intermediate/Helper.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static constexpr unsigned IMAGE_CONFIG_NUM_UNIFORMS {4};

//The first entry is the base texture setup
const Value IMAGE_CONFIG_BASE_OFFSET(INT_ZERO);
//The second entry is the texture access setup
const Value IMAGE_CONFIG_ACCESS_OFFSET(Literal(static_cast<unsigned long>(sizeof(unsigned))), TYPE_INT32);
//The third entry is the extended texture setup (e.g. cube map, child images, etc.)
const Value IMAGE_CONFIG_EXTENDED_OFFSET(Literal(2 * static_cast<unsigned long>(sizeof(unsigned))), TYPE_INT32);
//The forth entry is the original OpenCL image- and channel-type configuration
const Value IMAGE_CONFIG_CHANNEL_OFFSET(Literal(3 * static_cast<unsigned long>(sizeof(unsigned))), TYPE_INT32);

Global* intermediate::reserveImageConfiguration(Module& module, const Value& image)
{
	if(!image.type.getImageType().hasValue)
		throw CompilationError(CompilationStep::GENERAL, "Can't reserve global data for image-configuration of non-image type", image.type.to_string());
	if(!image.hasType(ValueType::LOCAL))
		throw CompilationError(CompilationStep::GENERAL, "Cannot reserve global data for non-local image", image.to_string());
	logging::debug() << "Reserving a buffer of " << IMAGE_CONFIG_NUM_UNIFORMS << " UNIFORMs for the image-configuration of " << image.to_string() << logging::endl;
	auto it = module.globalData.emplace(module.globalData.end(), Global(ImageType::toImageConfigurationName(image.local->name), TYPE_INT32.toVectorType(IMAGE_CONFIG_NUM_UNIFORMS), INT_ZERO));
	return &(*it);
}

InstructionWalker intermediate::intrinsifyImageFunction(InstructionWalker it, Method& method)
{
	MethodCall* callSite = it.get<MethodCall>();
	if(callSite != nullptr)
	{
		if(callSite->methodName.find("vc4cl_sampler_get_normalized_coords") != std::string::npos)
		{
			logging::debug() << "Intrinsifying getting normalized-coordinates flag from sampler" << logging::endl;
			it.reset(new Operation("and", callSite->getOutput(), callSite->getArgument(0), Value(Literal(Sampler::MASK_NORMALIZED_COORDS), TYPE_INT8)));
		}
		else if(callSite->methodName.find("vc4cl_sampler_get_addressing_mode") != std::string::npos)
		{
			logging::debug() << "Intrinsifying getting addressing-mode flag from sampler" << logging::endl;
			it.reset(new Operation("and", callSite->getOutput(), callSite->getArgument(0), Value(Literal(Sampler::MASK_ADDRESSING_MODE), TYPE_INT8)));
		}
		else if(callSite->methodName.find("vc4cl_sampler_get_filter_mode") != std::string::npos)
		{
			logging::debug() << "Intrinsifying getting filter-mode flag from sampler" << logging::endl;
			it.reset(new Operation("and", callSite->getOutput(), callSite->getArgument(0), Value(Literal(Sampler::MASK_FILTER_MODE), TYPE_INT8)));
		}
	}
	return it;
}

static InstructionWalker insertLoadImageConfig(InstructionWalker it, Method& method, const Value& image, const Value& dest, const Value& offset)
{
	const Global* imageConfig = method.findGlobal(ImageType::toImageConfigurationName(image.local->name));
	if(imageConfig == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Image-configuration is not yet reserved!");
	const Value addrTemp = method.addNewLocal(TYPE_INT32.toPointerType(), "%image_config");
	it.emplace(new Operation("add", addrTemp, imageConfig->createReference(), offset));
	it.nextInBlock();
	it = periphery::insertReadDMA(method, it, dest, addrTemp);
	return it;
}

InstructionWalker intermediate::insertQueryChannelDataType(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	//upper half of the channel-info field
	const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_CHANNEL_OFFSET);
	it.emplace(new Operation("shr", dest, valTemp, Value(Literal(16L), TYPE_INT8)));
	it.nextInBlock();
	return it;
}

InstructionWalker intermediate::insertQueryChannelOrder(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	//lower half of the channel-info field
	const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_CHANNEL_OFFSET);
	it.emplace(new Operation("and", dest, valTemp, Value(Literal(0xFFFFL), TYPE_INT16)));
	it.nextInBlock();
	return it;
}

static InstructionWalker insertLoadImageWidth(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_ACCESS_OFFSET);
	const Value widthTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it.emplace(new Operation("shr", widthTemp, valTemp, Value(Literal(8L), TYPE_INT8)));
	it.nextInBlock();
	it.emplace(new Operation("and", dest, widthTemp, Value(Literal(static_cast<long>(Bitfield<uint32_t>::MASK_Undecuple)), TYPE_INT32), COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	//0 => 2048
	it.emplace(new MoveOperation(dest, Value(Literal(2048L), TYPE_INT32), COND_ZERO_SET));
	it.nextInBlock();
	return it;
}

static InstructionWalker insertLoadImageHeight(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_ACCESS_OFFSET);
	const Value heightTemp = method.addNewLocal(TYPE_INT32, "%image_config");
	it.emplace(new Operation("shr", heightTemp, valTemp, Value(Literal(20L), TYPE_INT8)));
	it.nextInBlock();
	it.emplace(new Operation("and", dest, heightTemp, Value(Literal(static_cast<long>(Bitfield<uint32_t>::MASK_Undecuple)), TYPE_INT32), COND_ALWAYS, SetFlag::SET_FLAGS));
	it.nextInBlock();
	//0 => 2048
	it.emplace(new MoveOperation(dest, Value(Literal(2048L), TYPE_INT32), COND_ZERO_SET));
	it.nextInBlock();
	return it;
}

InstructionWalker intermediate::insertQueryMeasurements(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	if(!image.type.getImageType().hasValue)
		throw CompilationError(CompilationStep::GENERAL, "Can't query image measurements from non-image object", image.to_string());
	//TODO queries the measurements of the image
	//depending on number of dimensions, load x int16 values,
	//or always load all 4 possible dimensions
	//XXX for arrays, e.g. 1D-array, second dimension is array-size, not forth
	//-> store dimensions in an extra field something like e.g. for 1D-array x, length, for 2D-array x, y, length, ... ??
	//or depending on image type (known here!), create different load instruction
	//available types: 1D, 1D array, 2D, 2D array, 3D
	const ImageType* imageType = image.type.getImageType().get();
	if(imageType->isImageArray)
		//XXX need to add a dimension, where to get the array-size from?
		throw CompilationError(CompilationStep::GENERAL, "Image-arrays are not supported yet", image.to_string());
	if(imageType->dimensions == 1)
	{
		return insertLoadImageWidth(it, method, image, dest);
	}
	else if(imageType->dimensions == 2)
	{
		const Value imgWidth = method.addNewLocal(TYPE_INT32, "%image_width");
		const Value imgHeight = method.addNewLocal(TYPE_INT32, "%image_height");
		it = insertLoadImageWidth(it, method, image, imgWidth);
		it = insertLoadImageHeight(it, method, image, imgHeight);
		Value mask(ContainerValue(), TYPE_INT8);
		mask.container.elements.push_back(INT_ZERO);
		mask.container.elements.push_back(INT_ONE);
		return insertVectorShuffle(it, method, dest, imgWidth, imgHeight, mask);
	}
	//TODO how to get image depth? sub-images?
	/*
	else if(imageType->dimensions == 3)
	{

	}
	*/
	else
		throw CompilationError(CompilationStep::GENERAL, "Unsupported image dimensions", std::to_string(static_cast<unsigned>(imageType->dimensions)));
	throw CompilationError(CompilationStep::GENERAL, "Unimplemented image-query function", it->to_string());
}
