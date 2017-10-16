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

static InstructionWalker insertQueryPitches(InstructionWalker it, Method& method, const Value& image, const Value& pitches)
{
	//0. check if row/slice pitch was already retrieved
	const std::string localPitches = image.local->name + ".pitches";
	Value resultPitches(UNDEFINED_VALUE);
	if(method.findLocal(localPitches) != nullptr)
	{
		resultPitches = method.findLocal(localPitches)->createReference();
	}
	else
	{
		//1. calculate offset
		const Value addr = method.addNewLocal(TYPE_INT32, "%image_query_addr");
		Value tmp = method.addNewLocal(IMAGE_SIZE_TYPE, "%image_query_pitch");
		resultPitches = method.addNewLocal(IMAGE_SIZE_TYPE, image.local->name, "pitches");
		//grab both pitches at the same time -> 2 x short
		tmp.type.num = 2;
		it.emplace(new Operation("add", addr, image, IMAGE_DIMENSIONS_OFFSET));
		it.nextInBlock();
		//2. retrieve value
		it = periphery::insertReadDMA(method, it, tmp, addr, true);
	}
	return insertZeroExtension(it, method, resultPitches, pitches);
}

InstructionWalker intermediate::intrinsifyImageFunction(InstructionWalker it, Method& method)
{
	MethodCall* callSite = it.get<MethodCall>();
	if(callSite != nullptr)
	{
		if(callSite->methodName.find("vc4cl_image_get_pitches") != std::string::npos)
		{
			logging::debug() << "intrinsifying retrieving image pitches" << logging::endl;
			it = insertQueryPitches(it, method, callSite->getArgument(0), callSite->getOutput());
			it.erase();
			//so next instruction is not skipped
			it.previousInBlock();
		}
		else if(callSite->methodName.find("vc4cl_image_get_data_address") != std::string::npos)
		{
			logging::debug() << "Intrinsifying calculating image data address" << logging::endl;
			it.reset(new Operation("add", callSite->getOutput(), callSite->getArgument(0), IMAGE_DATA_OFFSET));
		}
		else if(callSite->methodName.find("vc4cl_image_read_pixel") != std::string::npos)
		{
			throw CompilationError(CompilationStep::LLVM_2_IR, "Unsupported intrinsic", callSite->to_string());
		}
		else if(callSite->methodName.find("vc4cl_image_write_pixel") != std::string::npos)
		{
			throw CompilationError(CompilationStep::LLVM_2_IR, "Unsupported intrinsic", callSite->to_string());
		}
		else if(callSite->methodName.find("vc4cl_sampler_get_normalized_coords") != std::string::npos)
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

InstructionWalker intermediate::insertQueryChannelDataType(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	//0. check if the channel-type was already retrieved for this image
	const std::string localName = image.local->name + ".channel_data_type";
	Value result(UNDEFINED_VALUE);
	if(method.findLocal(localName) != nullptr)
	{
		result = method.findLocal(localName)->createReference();
	}
	else
	{
		//1. calculate offset
		const Value addr = method.addNewLocal(TYPE_INT32, "%image_query_addr");
		result = method.addNewLocal(IMAGE_INFO_TYPE, image.local->name, "channel_data_type");
		it.emplace( new Operation("add", addr, image, IMAGE_CHANNEL_TYPE_OFFSET));
		it.nextInBlock();
		//2. retrieve value
		it = periphery::insertReadDMA(method, it, result, addr, true);
	}
	return insertZeroExtension( it, method, result, dest);
}

InstructionWalker intermediate::insertQueryChannelOrder(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	//0. check if the channel-order was already retrieved for this image
	const std::string localName = image.local->name + ".channel_order";
	Value result(UNDEFINED_VALUE);
	if(method.findLocal(localName) != nullptr)
	{
		result = method.findLocal(localName)->createReference();
	}
	else
	{
		//1. calculate offset
		const Value addr = method.addNewLocal(TYPE_INT32, "%image_query_addr");
		result = method.addNewLocal(IMAGE_INFO_TYPE, image.local->name, "channel_order");
		it.emplace( new Operation("add", addr, image, IMAGE_CHANNEL_ORDER_OFFSET));
		it.nextInBlock();
		//2. retrieve value
		it = periphery::insertReadDMA(method,  it, result, addr, true);
	}
	return insertZeroExtension(it, method, result, dest);
}

InstructionWalker intermediate::insertQueryMeasurements(InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
	//0. check if measurements were already retrieved
	const std::string localName = image.local->name + ".measurements";
	Value result(UNDEFINED_VALUE);
	if(method.findLocal(localName) != nullptr)
	{
		result = method.findLocal(localName)->createReference();
	}
	else
	{
		//1. calculate offset
		const Value addr = method.addNewLocal(TYPE_INT32, "%image_query_addr");
		result = method.addNewLocal(IMAGE_SIZE_TYPE, image.local->name, "measurements");
		//TODO queries the measurements of the image
		//depending on number of dimensions, load x int16 values,
		//or always load all 4 possible dimensions
		//XXX for arrays, e.g. 1D-array, second dimension is array-size, not forth
		//-> store dimensions something like e.g. for 1D-array x, length, for 2D-array x, y, length, ... ??
		it.emplace(new Operation("add", addr, image, IMAGE_DIMENSIONS_OFFSET));
		it.nextInBlock();
		//2. retrieve value
		it = periphery::insertReadDMA(method, it, result, addr, true);
	}
	return insertZeroExtension(it, method, result, dest);
}
