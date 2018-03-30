/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Images.h"

#include "../Module.h"
#include "../intermediate/Helper.h"
#include "../periphery/VPM.h"
#include "log.h"

using namespace vc4c;
using namespace vc4c::intermediate;

static constexpr unsigned IMAGE_CONFIG_NUM_UNIFORMS{6};

// The first entry is the base texture setup
const Value IMAGE_CONFIG_BASE_OFFSET(INT_ZERO);
// The second entry is the texture access setup
const Value IMAGE_CONFIG_ACCESS_OFFSET(Literal(static_cast<uint32_t>(sizeof(unsigned))), TYPE_INT32);
// The third entry is the extended texture setup (e.g. cube map, child images, etc.)
const Value IMAGE_CONFIG_CHILD_DIMENSION_OFFSET(Literal(2 * static_cast<uint32_t>(sizeof(unsigned))), TYPE_INT32);
// The forth entry is the second extended texture setup (e.g. child image offsets)
const Value IMAGE_CONFIG_CHILD_OFFSET_OFFSET(Literal(3 * static_cast<uint32_t>(sizeof(unsigned))), TYPE_INT32);
// The fifth entry is the original OpenCL image- and channel-type configuration
const Value IMAGE_CONFIG_CHANNEL_OFFSET(Literal(4 * static_cast<uint32_t>(sizeof(unsigned))), TYPE_INT32);
// The sixth entry is the array-size or image-depth
const Value IMAGE_CONFIG_ARRAY_SIZE_OFFSET(Literal(5 * static_cast<uint32_t>(sizeof(unsigned))), TYPE_INT32);
const Value IMAGE_CONFIG_IMAGE_DEPTH_OFFSET = IMAGE_CONFIG_ARRAY_SIZE_OFFSET;

Global* intermediate::reserveImageConfiguration(Module& module, Parameter& image)
{
    // TODO find a better/more central way to reserve space for image configurations (not in every front-end)
    // XXX also make sure, a config for every image is only created once
    if(!image.type.getImageType())
        throw CompilationError(CompilationStep::GENERAL,
            "Can't reserve global data for image-configuration of non-image type", image.type.to_string());
    logging::debug() << "Reserving a buffer of " << IMAGE_CONFIG_NUM_UNIFORMS
                     << " UNIFORMs for the image-configuration of " << image.to_string() << logging::endl;
    auto it = module.globalData.emplace(module.globalData.end(),
        Global(ImageType::toImageConfigurationName(image.name),
            TYPE_INT32.toVectorType(IMAGE_CONFIG_NUM_UNIFORMS).toPointerType(),
            Value(Literal(0u), TYPE_INT32.toVectorType(IMAGE_CONFIG_NUM_UNIFORMS)), false));
    return &(*it);
}

static InstructionWalker insertLoadImageConfig(
    InstructionWalker it, Method& method, const Value& image, const Value& dest, const Value& offset)
{
    const Global* imageConfig = method.findGlobal(ImageType::toImageConfigurationName(image.local->name));
    if(imageConfig == nullptr)
        throw CompilationError(CompilationStep::GENERAL, "Image-configuration is not yet reserved", image.to_string());
    const Value addrTemp = method.addNewLocal(TYPE_INT32.toPointerType(), "%image_config");
    it.emplace(new Operation(OP_ADD, addrTemp, imageConfig->createReference(), offset));
    it.nextInBlock();
    it = periphery::insertReadVectorFromTMU(method, it, dest, addrTemp);
    return it;
}

static InstructionWalker insertLoadArraySizeOrImageDepth(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    return insertLoadImageConfig(it, method, image, dest, IMAGE_CONFIG_ARRAY_SIZE_OFFSET);
}

InstructionWalker intermediate::intrinsifyImageFunction(InstructionWalker it, Method& method)
{
    MethodCall* callSite = it.get<MethodCall>();
    if(callSite != nullptr)
    {
        if(callSite->methodName.find("vc4cl_sampler_get_normalized_coords") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting normalized-coordinates flag from sampler" << logging::endl;
            it.reset(new Operation(OP_AND, callSite->getOutput().value(), callSite->getArgument(0).value(),
                Value(Literal(Sampler::MASK_NORMALIZED_COORDS), TYPE_INT8)));
        }
        else if(callSite->methodName.find("vc4cl_sampler_get_addressing_mode") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting addressing-mode flag from sampler" << logging::endl;
            it.reset(new Operation(OP_AND, callSite->getOutput().value(), callSite->getArgument(0).value(),
                Value(Literal(Sampler::MASK_ADDRESSING_MODE), TYPE_INT8)));
        }
        else if(callSite->methodName.find("vc4cl_sampler_get_filter_mode") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting filter-mode flag from sampler" << logging::endl;
            it.reset(new Operation(OP_AND, callSite->getOutput().value(), callSite->getArgument(0).value(),
                Value(Literal(Sampler::MASK_FILTER_MODE), TYPE_INT8)));
        }
        else if(callSite->methodName.find("vc4cl_image_basic_setup") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting image basic setup: " << callSite->getArgument(0)->to_string()
                             << logging::endl;
            it = insertLoadImageConfig(
                it, method, callSite->getArgument(0).value(), callSite->getOutput().value(), IMAGE_CONFIG_BASE_OFFSET);
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("vc4cl_image_access_setup") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting image access setup: " << callSite->getArgument(0)->to_string()
                             << logging::endl;
            it = insertLoadImageConfig(it, method, callSite->getArgument(0).value(), callSite->getOutput().value(),
                IMAGE_CONFIG_ACCESS_OFFSET);
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("vc4cl_image_extended_setup") != std::string::npos)
        {
            logging::debug() << "Intrinsifying getting image extended setup: " << callSite->getArgument(0)->to_string()
                             << logging::endl;
            it = insertLoadImageConfig(it, method, callSite->getArgument(0).value(), callSite->getOutput().value(),
                IMAGE_CONFIG_CHILD_OFFSET_OFFSET);
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("get_image_channel_data_type") != std::string::npos)
        {
            logging::debug() << "Generating query of image's channel data-type for image: "
                             << callSite->getArgument(0)->to_string() << logging::endl;
            it =
                insertQueryChannelDataType(it, method, callSite->getArgument(0).value(), callSite->getOutput().value());
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("get_image_channel_order") != std::string::npos)
        {
            logging::debug() << "Generating query of image's channel order for image: "
                             << callSite->getArgument(0)->to_string() << logging::endl;
            it = insertQueryChannelOrder(it, method, callSite->getArgument(0).value(), callSite->getOutput().value());
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("get_image_depth") != std::string::npos)
        {
            logging::debug() << "Generating query of image's depth for image: " << callSite->getArgument(0)->to_string()
                             << logging::endl;
            it = insertLoadArraySizeOrImageDepth(
                it, method, callSite->getArgument(0).value(), callSite->getOutput().value());
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
        else if(callSite->methodName.find("get_image_array_size") != std::string::npos)
        {
            logging::debug() << "Generating query of image-array's size for image: "
                             << callSite->getArgument(0)->to_string() << logging::endl;
            it = insertLoadArraySizeOrImageDepth(
                it, method, callSite->getArgument(0).value(), callSite->getOutput().value());
            it.erase();
            // to not skip next instruction
            it.previousInBlock();
        }
    }
    return it;
}

InstructionWalker intermediate::insertQueryChannelDataType(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    // upper half of the channel-info field
    const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_CHANNEL_OFFSET);
    it.emplace(new Operation(OP_SHR, dest, valTemp, Value(Literal(16u), TYPE_INT8)));
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertQueryChannelOrder(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    // lower half of the channel-info field
    const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_CHANNEL_OFFSET);
    it.emplace(new Operation(OP_AND, dest, valTemp, Value(Literal(0xFFFFu), TYPE_INT16)));
    it.nextInBlock();
    return it;
}

static InstructionWalker insertLoadImageWidth(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_ACCESS_OFFSET);
    const Value widthTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it.emplace(new Operation(OP_SHR, widthTemp, valTemp, Value(Literal(8u), TYPE_INT8)));
    it.nextInBlock();
    it.emplace(new Operation(OP_AND, dest, widthTemp, Value(Literal(Bitfield<uint32_t>::MASK_Undecuple), TYPE_INT32),
        COND_ALWAYS, SetFlag::SET_FLAGS));
    it.nextInBlock();
    // 0 => 2048
    it.emplace(new MoveOperation(dest, Value(Literal(2048u), TYPE_INT32), COND_ZERO_SET));
    it.nextInBlock();
    return it;
}

static InstructionWalker insertLoadImageHeight(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    const Value valTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it = insertLoadImageConfig(it, method, image, valTemp, IMAGE_CONFIG_ACCESS_OFFSET);
    const Value heightTemp = method.addNewLocal(TYPE_INT32, "%image_config");
    it.emplace(new Operation(OP_SHR, heightTemp, valTemp, Value(Literal(20u), TYPE_INT8)));
    it.nextInBlock();
    it.emplace(new Operation(OP_AND, dest, heightTemp, Value(Literal(Bitfield<uint32_t>::MASK_Undecuple), TYPE_INT32),
        COND_ALWAYS, SetFlag::SET_FLAGS));
    it.nextInBlock();
    // 0 => 2048
    it.emplace(new MoveOperation(dest, Value(Literal(2048u), TYPE_INT32), COND_ZERO_SET));
    it.nextInBlock();
    return it;
}

InstructionWalker intermediate::insertQueryMeasurements(
    InstructionWalker it, Method& method, const Value& image, const Value& dest)
{
    if(!image.type.getImageType())
        throw CompilationError(
            CompilationStep::GENERAL, "Can't query image measurements from non-image object", image.to_string());
    // available types: 1D, 1D array, 2D, 2D array, 3D
    const ImageType* imageType = image.type.getImageType().value();
    if(imageType->dimensions == 1)
    {
        if(imageType->isImageArray)
        {
            const Value imgWidth = method.addNewLocal(TYPE_INT32, "%image_width");
            const Value arraySize = method.addNewLocal(TYPE_INT32, "%image_array_size");
            it = insertLoadImageWidth(it, method, image, imgWidth);
            it = insertLoadArraySizeOrImageDepth(it, method, image, arraySize);
            Value mask(ContainerValue({INT_ZERO, INT_ONE}), TYPE_INT8.toVectorType(2));
            return insertVectorShuffle(it, method, dest, imgWidth, arraySize, mask);
        }
        return insertLoadImageWidth(it, method, image, dest);
    }
    else if(imageType->dimensions == 2)
    {
        const Value imgWidth = method.addNewLocal(TYPE_INT32, "%image_width");
        const Value imgHeight = method.addNewLocal(TYPE_INT32, "%image_height");
        it = insertLoadImageWidth(it, method, image, imgWidth);
        it = insertLoadImageHeight(it, method, image, imgHeight);
        Value mask(ContainerValue({INT_ZERO, INT_ONE}), TYPE_INT8.toVectorType(2));
        if(imageType->isImageArray)
        {
            // XXX OpenCL C function get_image_dim() for image_2d_array_t does not return the array-size (only width and
            // height)
            const Value tmp = method.addNewLocal(TYPE_INT32.toVectorType(2), "%image_dimensions");
            it = insertVectorShuffle(it, method, tmp, imgWidth, imgHeight, mask);

            const Value arraySize = method.addNewLocal(TYPE_INT32, "%image_array_size");
            it = insertLoadArraySizeOrImageDepth(it, method, image, arraySize);

            mask = Value(ContainerValue({INT_ZERO, INT_ONE, Value(Literal(2u), TYPE_INT8)}), TYPE_INT8.toVectorType(3));
            return insertVectorShuffle(it, method, dest, tmp, arraySize, mask);
        }
        return insertVectorShuffle(it, method, dest, imgWidth, imgHeight, mask);
    }
    else if(imageType->dimensions == 3)
    {
        const Value imgWidth = method.addNewLocal(TYPE_INT32, "%image_width");
        const Value imgHeight = method.addNewLocal(TYPE_INT32, "%image_height");
        it = insertLoadImageWidth(it, method, image, imgWidth);
        it = insertLoadImageHeight(it, method, image, imgHeight);
        Value mask(ContainerValue({INT_ZERO, INT_ONE}), TYPE_INT8.toVectorType(2));

        const Value tmp = method.addNewLocal(TYPE_INT32.toVectorType(2), "%image_dimensions");
        it = insertVectorShuffle(it, method, tmp, imgWidth, imgHeight, mask);

        const Value arraySize = method.addNewLocal(TYPE_INT32, "%image_depth");
        it = insertLoadArraySizeOrImageDepth(it, method, image, arraySize);

        mask = Value(ContainerValue({INT_ZERO, INT_ONE, Value(Literal(2u), TYPE_INT8)}), TYPE_INT8.toVectorType(3));
        return insertVectorShuffle(it, method, dest, tmp, arraySize, mask);
    }
    else
        throw CompilationError(CompilationStep::GENERAL, "Unsupported image dimensions",
            std::to_string(static_cast<unsigned>(imageType->dimensions)));
    throw CompilationError(CompilationStep::GENERAL, "Unimplemented image-query function", it->to_string());
}
