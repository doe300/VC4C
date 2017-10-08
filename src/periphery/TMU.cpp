/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TMU.h"
#include "../InstructionWalker.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::periphery;

std::string TextureType::to_string() const
{
	switch(value)
	{
		case RGBA8888.value:
			return "RGBA 32-bit";
		case RGBX8888.value:
			return "RGB 24-bit";
		case RGBA4444.value:
			return "RGBA 16-bit";
		case RGBA5551.value:
			return "RGBA 16-bit 5-5-5-1";
		case RGB565.value:
			return "RGB 16-bit 5-6-5";
		case LUMINANCE.value:
			return "luminance 8-bit";
		case ALPHA.value:
			return "alpha 8-bit";
		case LUMALPHA.value:
			return "luminance, alpha 16-bit";
		case ECT1.value:
			return "Ericsson Texture Compression";
		case S16F.value:
			return "float 16-bit";
		case S8.value:
			return "int 8-bit";
		case S16.value:
			return "int 16-bit";
		case BW1.value:
			return "black-white 1-bit";
		case A4.value:
			return "alpha 4-bit";
		case A1.value:
			return "alpha 1-bit";
		case RGBA64.value:
			return "RGBA float 64-bit";
		case RGBA32R.value:
			return "RGBA raster, 32-bit";
		case YUYV422R.value:
			return "YUYV raster, 32-bit";
	}
	throw CompilationError(CompilationStep::GENERAL, "Unhandled texture-type", std::to_string(value));
}

Global* periphery::reserveImageConfiguration(Module& module, const Value& image)
{
	if(!image.type.getImageType().hasValue)
		throw CompilationError(CompilationStep::GENERAL, "Can't reserve global data for image-configuration of non-image type", image.type.to_string());
	if(!image.hasType(ValueType::LOCAL))
		throw CompilationError(CompilationStep::GENERAL, "Cannot reserve global data for non-local image", image.to_string());
	//TODO 3 UNIFORMS for image-array and 3D image?
	const unsigned char bufferSize = image.type.getImageType().get()->dimensions > 2 || image.type.getImageType().get()->isImageArray ? 3 : 2;
	logging::debug() << "Reserving a buffer of " << static_cast<unsigned>(bufferSize) << " UNIFORMs for the image-configuration of " << image.to_string() << logging::endl;
	auto it = module.globalData.emplace(module.globalData.end(), Global(ImageType::toImageConfigurationName(image.local->name), TYPE_INT32.toVectorType(bufferSize), INT_ZERO));
	return &(*it);
}

InstructionWalker periphery::insertGeneralReadTMU(InstructionWalker it, const Value& dest, const Value& addr)
{
	//TODO mutex lock required?

	it.emplace(new intermediate::MoveOperation(TMU_GENERAL_READ_ADDRESS, addr));
	it.nextInBlock();
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(Signaling::LOAD_TMU0);
	it.nextInBlock();
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	return it;
}

InstructionWalker periphery::insertReadTMU(Method& method, InstructionWalker it, const Value& image, const Value& dest, const Value& xCoord, const Optional<Value>& yCoord)
{
	//TODO mutex lock required?
	if(!image.hasType(ValueType::LOCAL))
		throw CompilationError(CompilationStep::GENERAL, "Cannot access image-configuration for non-local image", image.to_string());
	const Global* imageConfig = method.findGlobal(ImageType::toImageConfigurationName(image.local->name));
	if(imageConfig == nullptr)
		throw CompilationError(CompilationStep::GENERAL, "Failed to find the image-configuration for", image.to_string());
	if(!xCoord.type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the x-axis", xCoord.to_string());
	if(yCoord.hasValue && !yCoord.get().type.isFloatingType())
		throw CompilationError(CompilationStep::GENERAL, "Can only read with floating-point coordinates in the y-axis", yCoord.to_string());

	// 1. set the UNIFORM pointer to point to the configurations for the image about to be read
	it.emplace(new intermediate::MoveOperation(Value(REG_UNIFORM_ADDRESS, TYPE_INT32.toVectorType(16).toPointerType()), imageConfig->createReference()));
	it.nextInBlock();
	// 2. need to wait 2 instructions for UNIFORM-pointer to be changed
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_UNIFORM));
	it.nextInBlock();
	// 3. write the TMU addresses
	if(yCoord.hasValue)
	{
		it.emplace(new intermediate::MoveOperation(TMU_COORD_T_REGISTER, yCoord));
		it.nextInBlock();
	}
	it.emplace(new intermediate::MoveOperation(TMU_COORD_S_REGISTER, xCoord));
	it.nextInBlock();
	// 4. trigger loadtmu
	it.emplace(new intermediate::Nop(intermediate::DelayType::WAIT_TMU));
	it->setSignaling(Signaling::LOAD_TMU0);
	it.nextInBlock();
	// 5. read from r4 (stalls 9 to 20 cycles)
	it.emplace(new intermediate::MoveOperation(dest, TMU_READ_REGISTER));
	it.nextInBlock();
	// 6. TODO reset UNIFORM pointer? for next work-group iteration, or disable when used with images?

	return it;
}
