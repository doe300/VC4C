/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVHelper.h"

#include "../performance.h"
#include "CompilationError.h"
#include "log.h"

#ifdef SPIRV_FRONTEND
#if __has_include("spirv-tools/linker.hpp")
#include "spirv-tools/linker.hpp"
#endif

#include <algorithm>

#ifdef __GNUC__
#include <cxxabi.h>
#endif

using namespace vc4c;
using namespace vc4c::spirv2qasm;

std::string spirv2qasm::getOpenCLMethodName(const uint32_t instructionID)
{
    switch(static_cast<OpenCLLIB::Entrypoints>(instructionID))
    {
    case OpenCLLIB::Entrypoints::Acos:
        return "acos";
    case OpenCLLIB::Entrypoints::Acosh:
        return "acosh";
    case OpenCLLIB::Entrypoints::Acospi:
        return "acospi";
    case OpenCLLIB::Entrypoints::Asin:
        return "asin";
    case OpenCLLIB::Entrypoints::Asinh:
        return "asinh";
    case OpenCLLIB::Entrypoints::Asinpi:
        return "asinpi";
    case OpenCLLIB::Entrypoints::Atan:
        return "atan";
    case OpenCLLIB::Entrypoints::Atan2:
        return "atan2";
    case OpenCLLIB::Entrypoints::Atan2pi:
        return "atan2pi";
    case OpenCLLIB::Entrypoints::Atanh:
        return "atanh";
    case OpenCLLIB::Entrypoints::Atanpi:
        return "atanpi";
    case OpenCLLIB::Entrypoints::Bitselect:
        return "bitselect";
    case OpenCLLIB::Entrypoints::Cbrt:
        return "cbrt";
    case OpenCLLIB::Entrypoints::Ceil:
        return "ceil";
    case OpenCLLIB::Entrypoints::Clz:
        return "clz";
    case OpenCLLIB::Entrypoints::Copysign:
        return "copysign";
    case OpenCLLIB::Entrypoints::Cos:
        return "cos";
    case OpenCLLIB::Entrypoints::Cosh:
        return "cosh";
    case OpenCLLIB::Entrypoints::Cospi:
        return "cospi";
    case OpenCLLIB::Entrypoints::Cross:
        return "cross";
    case OpenCLLIB::Entrypoints::Ctz:
        return "ctz";
    case OpenCLLIB::Entrypoints::Degrees:
        return "degrees";
    case OpenCLLIB::Entrypoints::Distance:
        return "distance";
    case OpenCLLIB::Entrypoints::Erf:
        return "erf";
    case OpenCLLIB::Entrypoints::Erfc:
        return "erfc";
    case OpenCLLIB::Entrypoints::Exp:
        return "exp";
    case OpenCLLIB::Entrypoints::Exp10:
        return "exp10";
    case OpenCLLIB::Entrypoints::Exp2:
        return "exp2";
    case OpenCLLIB::Entrypoints::Expm1:
        return "expm1";
    case OpenCLLIB::Entrypoints::FClamp:
        return "clamp";
    case OpenCLLIB::Entrypoints::FMax_common:
        return "max";
    case OpenCLLIB::Entrypoints::FMin_common:
        return "min";
    case OpenCLLIB::Entrypoints::Fabs:
        return "fabs";
    case OpenCLLIB::Entrypoints::Fast_distance:
        return "fast_distance";
    case OpenCLLIB::Entrypoints::Fast_length:
        return "fast_length";
    case OpenCLLIB::Entrypoints::Fast_normalize:
        return "fast_normalize";
    case OpenCLLIB::Entrypoints::Fdim:
        return "fdim";
    case OpenCLLIB::Entrypoints::Floor:
        return "floor";
    case OpenCLLIB::Entrypoints::Fma:
        return "fma";
    case OpenCLLIB::Entrypoints::Fmax:
        return "max";
    case OpenCLLIB::Entrypoints::Fmin:
        return "min";
    case OpenCLLIB::Entrypoints::Fmod:
        return "fmod";
    case OpenCLLIB::Entrypoints::Fract:
        return "fract";
    case OpenCLLIB::Entrypoints::Frexp:
        return "frexp";
    case OpenCLLIB::Entrypoints::Half_cos:
        return "half_cos";
    case OpenCLLIB::Entrypoints::Half_divide:
        return "half_divide";
    case OpenCLLIB::Entrypoints::Half_exp:
        return "half_exp";
    case OpenCLLIB::Entrypoints::Half_exp10:
        return "half_exp10";
    case OpenCLLIB::Entrypoints::Half_exp2:
        return "half_exp2";
    case OpenCLLIB::Entrypoints::Half_log:
        return "half_log";
    case OpenCLLIB::Entrypoints::Half_log10:
        return "half_log10";
    case OpenCLLIB::Entrypoints::Half_log2:
        return "half_log2";
    case OpenCLLIB::Entrypoints::Half_powr:
        return "half_powr";
    case OpenCLLIB::Entrypoints::Half_recip:
        return "half_recip";
    case OpenCLLIB::Entrypoints::Half_rsqrt:
        return "half_rsqrt";
    case OpenCLLIB::Entrypoints::Half_sin:
        return "half_sin";
    case OpenCLLIB::Entrypoints::Half_sqrt:
        return "half_sqrt";
    case OpenCLLIB::Entrypoints::Half_tan:
        return "half_tan";
    case OpenCLLIB::Entrypoints::Hypot:
        return "hypot";
    case OpenCLLIB::Entrypoints::Ilogb:
        return "ilogb";
    case OpenCLLIB::Entrypoints::Ldexp:
        return "ldexp";
    case OpenCLLIB::Entrypoints::Length:
        return "length";
    case OpenCLLIB::Entrypoints::Lgamma:
        return "lgamma";
    case OpenCLLIB::Entrypoints::Lgamma_r:
        return "lgamma_r";
    case OpenCLLIB::Entrypoints::Log:
        return "log";
    case OpenCLLIB::Entrypoints::Log10:
        return "log10";
    case OpenCLLIB::Entrypoints::Log1p:
        return "log1p";
    case OpenCLLIB::Entrypoints::Log2:
        return "log2";
    case OpenCLLIB::Entrypoints::Logb:
        return "logb";
    case OpenCLLIB::Entrypoints::Mad:
        return "mad";
    case OpenCLLIB::Entrypoints::Maxmag:
        return "maxmag";
    case OpenCLLIB::Entrypoints::Minmag:
        return "minmag";
    case OpenCLLIB::Entrypoints::Mix:
        return "mix";
    case OpenCLLIB::Entrypoints::Modf:
        return "modf";
    case OpenCLLIB::Entrypoints::Nan:
        return "nan";
    case OpenCLLIB::Entrypoints::Native_cos:
        return "native_cos";
    case OpenCLLIB::Entrypoints::Native_divide:
        return "native_divide";
    case OpenCLLIB::Entrypoints::Native_exp:
        return "native_exp";
    case OpenCLLIB::Entrypoints::Native_exp10:
        return "native_exp10";
    case OpenCLLIB::Entrypoints::Native_exp2:
        return "native_exp2";
    case OpenCLLIB::Entrypoints::Native_log:
        return "native_log";
    case OpenCLLIB::Entrypoints::Native_log10:
        return "native_log10";
    case OpenCLLIB::Entrypoints::Native_log2:
        return "native_log2";
    case OpenCLLIB::Entrypoints::Native_powr:
        return "native_powr";
    case OpenCLLIB::Entrypoints::Native_recip:
        return "native_recip";
    case OpenCLLIB::Entrypoints::Native_rsqrt:
        return "rsqrt";
    case OpenCLLIB::Entrypoints::Native_sin:
        return "native_sin";
    case OpenCLLIB::Entrypoints::Native_sqrt:
        return "native_sqrt";
    case OpenCLLIB::Entrypoints::Native_tan:
        return "native_tan";
    case OpenCLLIB::Entrypoints::Nextafter:
        return "nextafter";
    case OpenCLLIB::Entrypoints::Normalize:
        return "normalize";
    case OpenCLLIB::Entrypoints::Popcount:
        return "popcount";
    case OpenCLLIB::Entrypoints::Pow:
        return "pow";
    case OpenCLLIB::Entrypoints::Pown:
        return "pown";
    case OpenCLLIB::Entrypoints::Powr:
        return "powr";
    case OpenCLLIB::Entrypoints::Prefetch:
        return "prefetch";
    case OpenCLLIB::Entrypoints::Printf:
        return "printf";
    case OpenCLLIB::Entrypoints::Radians:
        return "radians";
    case OpenCLLIB::Entrypoints::Remainder:
        return "remainder";
    case OpenCLLIB::Entrypoints::Remquo:
        return "remquo";
    case OpenCLLIB::Entrypoints::Rint:
        return "rint";
    case OpenCLLIB::Entrypoints::Rootn:
        return "rootn";
    case OpenCLLIB::Entrypoints::Rotate:
        return "rotate";
    case OpenCLLIB::Entrypoints::Round:
        return "round";
    case OpenCLLIB::Entrypoints::Rsqrt:
        return "rsqrt";
    case OpenCLLIB::Entrypoints::SAbs:
        return "abs";
    case OpenCLLIB::Entrypoints::SAbs_diff:
        return "abs_diff";
    case OpenCLLIB::Entrypoints::SAdd_sat:
        return "add_sat";
    case OpenCLLIB::Entrypoints::SClamp:
        return "clamp";
    case OpenCLLIB::Entrypoints::SHadd:
        return "hadd";
    case OpenCLLIB::Entrypoints::SMad24:
        return "mad24";
    case OpenCLLIB::Entrypoints::SMad_hi:
        return "mad_hi";
    case OpenCLLIB::Entrypoints::SMad_sat:
        return "mad_sat";
    case OpenCLLIB::Entrypoints::SMax:
        return "max";
    case OpenCLLIB::Entrypoints::SMin:
        return "min";
    case OpenCLLIB::Entrypoints::SMul24:
        return "mul24";
    case OpenCLLIB::Entrypoints::SMul_hi:
        return "mul_hi";
    case OpenCLLIB::Entrypoints::Smoothstep:
        return "smoothstep";
    case OpenCLLIB::Entrypoints::SRhadd:
        return "rhadd";
    case OpenCLLIB::Entrypoints::SSub_sat:
        return "sub_sat";
    case OpenCLLIB::Entrypoints::S_Upsample:
        return "upsample";
    case OpenCLLIB::Entrypoints::Select:
        return "select";
    case OpenCLLIB::Entrypoints::Shuffle:
        return "shuffle";
    case OpenCLLIB::Entrypoints::Shuffle2:
        return "shuffle2";
    case OpenCLLIB::Entrypoints::Sign:
        return "sign";
    case OpenCLLIB::Entrypoints::Sin:
        return "sin";
    case OpenCLLIB::Entrypoints::Sincos:
        return "sincos";
    case OpenCLLIB::Entrypoints::Sinh:
        return "sinh";
    case OpenCLLIB::Entrypoints::Sinpi:
        return "sinpi";
    case OpenCLLIB::Entrypoints::Sqrt:
        return "sqrt";
    case OpenCLLIB::Entrypoints::Step:
        return "step";
    case OpenCLLIB::Entrypoints::Tan:
        return "tan";
    case OpenCLLIB::Entrypoints::Tanh:
        return "tanh";
    case OpenCLLIB::Entrypoints::Tanpi:
        return "tanpi";
    case OpenCLLIB::Entrypoints::Tgamma:
        return "tgamma";
    case OpenCLLIB::Entrypoints::Trunc:
        return "trunc";
    case OpenCLLIB::Entrypoints::UAbs:
        return "abs";
    case OpenCLLIB::Entrypoints::UAbs_diff:
        return "abs_diff";
    case OpenCLLIB::Entrypoints::UAdd_sat:
        return "add_sat";
    case OpenCLLIB::Entrypoints::UClamp:
        return "clamp";
    case OpenCLLIB::Entrypoints::UHadd:
        return "hadd";
    case OpenCLLIB::Entrypoints::UMad24:
        return "mad24";
    case OpenCLLIB::Entrypoints::UMad_hi:
        return "mad_hi";
    case OpenCLLIB::Entrypoints::UMad_sat:
        return "mad_sat";
    case OpenCLLIB::Entrypoints::UMax:
        return "max";
    case OpenCLLIB::Entrypoints::UMin:
        return "min";
    case OpenCLLIB::Entrypoints::UMul24:
        return "mul24";
    case OpenCLLIB::Entrypoints::UMul_hi:
        return "mul_hi";
    case OpenCLLIB::Entrypoints::URhadd:
        return "rhadd";
    case OpenCLLIB::Entrypoints::USub_sat:
        return "sub_sat";
    case OpenCLLIB::Entrypoints::U_Upsample:
        return "upsample";
    case OpenCLLIB::Entrypoints::Vloadn:
        return "vloadn";
    case OpenCLLIB::Entrypoints::Vstoren:
        return "vstoren";
    default:
        throw CompilationError(
            CompilationStep::PARSER, "Unsupported OpenCL standard-function", std::to_string(instructionID));
    }
}

std::string spirv2qasm::getErrorMessage(spv_result_t error)
{
    switch(error)
    {
    case SPV_UNSUPPORTED:
        return "Unsupported operation";
    case SPV_END_OF_STREAM:
        return "End of Stream";
    case SPV_WARNING:
        return "Warning";
    case SPV_FAILED_MATCH:
        return "Failed match";
    case SPV_REQUESTED_TERMINATION:
        return "Requested Termination";
    case SPV_ERROR_INTERNAL:
        return "Internal Error";
    case SPV_ERROR_OUT_OF_MEMORY:
        return "Out of memory";
    case SPV_ERROR_INVALID_POINTER:
        return "Invalid pointer";
    case SPV_ERROR_INVALID_BINARY:
        return "Invalid binary input";
    case SPV_ERROR_INVALID_TEXT:
        return "Invalid text input";
    case SPV_ERROR_INVALID_TABLE:
        return "Invalid table";
    case SPV_ERROR_INVALID_VALUE:
        return "Invalid value";
    case SPV_ERROR_INVALID_DIAGNOSTIC:
        return "Invalid diagnostic";
    case SPV_ERROR_INVALID_LOOKUP:
        return "Invalid lookup";
    case SPV_ERROR_INVALID_ID:
        return "Invalid ID";
    case SPV_ERROR_INVALID_CFG:
        return "Invalid configuration";
    case SPV_ERROR_INVALID_LAYOUT:
        return "Invalid layout";
    case SPV_ERROR_INVALID_CAPABILITY:
        return "Invalid capability";
    case SPV_ERROR_INVALID_DATA:
        return "Invalid data";
    default:
        return "General error";
    }
}

static std::string getCapabilityName(const spv::Capability cap)
{
    switch(cap)
    {
    case spv::Capability::Matrix:
        return "Matrix";
    case spv::Capability::Shader:
        return "Shader";
    case spv::Capability::Geometry:
        return "Geometry";
    case spv::Capability::Tessellation:
        return "Tessellation";
    case spv::Capability::Addresses:
        return "Addresses";
    case spv::Capability::Linkage:
        return "Linkage";
    case spv::Capability::Kernel:
        return "Kernel";
    case spv::Capability::Vector16:
        return "Vector16";
    case spv::Capability::Float16Buffer:
        return "Float16Buffer";
    case spv::Capability::Float16:
        return "Float16";
    case spv::Capability::Float64:
        return "Float64";
    case spv::Capability::Int64:
        return "Int64";
    case spv::Capability::Int64Atomics:
        return "Int64Atomics";
    case spv::Capability::ImageBasic:
        return "ImageBasic";
    case spv::Capability::ImageReadWrite:
        return "ImageReadWrite";
    case spv::Capability::ImageMipmap:
        return "ImageMipmap";
    case spv::Capability::Pipes:
        return "Pipes";
    case spv::Capability::Groups:
        return "Groups";
    case spv::Capability::DeviceEnqueue:
        return "DeviceEnqueue";
    case spv::Capability::LiteralSampler:
        return "LiteralSampler";
    case spv::Capability::AtomicStorage:
        return "AtomicStorage";
    case spv::Capability::Int16:
        return "Int16";
    case spv::Capability::TessellationPointSize:
        return "TessellationPointSize";
    case spv::Capability::GeometryPointSize:
        return "GeometryPointSize";
    case spv::Capability::ImageGatherExtended:
        return "ImageGatherExtended";
    case spv::Capability::StorageImageMultisample:
        return "StorageOmageMultisample";
    case spv::Capability::UniformBufferArrayDynamicIndexing:
        return "UniformBifferArrayDynamicIndexing";
    case spv::Capability::SampledImageArrayDynamicIndexing:
        return "SampledImageArrayDynamicIndexing";
    case spv::Capability::StorageBufferArrayDynamicIndexing:
        return "StorageBufferArrayDynamicIndexing";
    case spv::Capability::StorageImageArrayDynamicIndexing:
        return "StorageImageArrayDynamicIndexing";
    case spv::Capability::ClipDistance:
        return "ClipDistance";
    case spv::Capability::CullDistance:
        return "CullDistance";
    case spv::Capability::ImageCubeArray:
        return "ImageCubeArray";
    case spv::Capability::SampleRateShading:
        return "SampleRateShading";
    case spv::Capability::ImageRect:
        return "ImageRect";
    case spv::Capability::SampledRect:
        return "SampledRect";
    case spv::Capability::GenericPointer:
        return "GenericPointer";
    case spv::Capability::Int8:
        return "Int8";
    case spv::Capability::InputAttachment:
        return "InputAttachment";
    case spv::Capability::SparseResidency:
        return "SparseResidency";
    case spv::Capability::MinLod:
        return "MinLod";
    case spv::Capability::Sampled1D:
        return "Sampled1D";
    case spv::Capability::Image1D:
        return "Image1D";
    case spv::Capability::SampledCubeArray:
        return "SampledCubeArray";
    case spv::Capability::SampledBuffer:
        return "SampledBuffer";
    case spv::Capability::ImageBuffer:
        return "ImageBuffer";
    case spv::Capability::ImageMSArray:
        return "ImageMSArray";
    case spv::Capability::StorageImageExtendedFormats:
        return "StorageImageExtendedFormats";
    case spv::Capability::ImageQuery:
        return "ImageQuery";
    case spv::Capability::DerivativeControl:
        return "DerivativeControl";
    case spv::Capability::InterpolationFunction:
        return "InterpolationFunction";
    case spv::Capability::TransformFeedback:
        return "TransformFeedback";
    case spv::Capability::GeometryStreams:
        return "GeometryStreams";
    case spv::Capability::StorageImageReadWithoutFormat:
        return "StorageImageReadWithoutFormat";
    case spv::Capability::StorageImageWriteWithoutFormat:
        return "StorageImageWriteWithoutFormat";
    case spv::Capability::MultiViewport:
        return "MultiViewport";
    case spv::Capability::SubgroupDispatch:
        return "SubgroupDispatch";
    case spv::Capability::NamedBarrier:
        return "NamedBarrier";
    case spv::Capability::PipeStorage:
        return "PipeStorage";
    case spv::Capability::SubgroupBallotKHR:
        return "SubgroupBallotKHR";
    case spv::Capability::DrawParameters:
        return "DrawParameters";
    case spv::Capability::SubgroupVoteKHR:
        return "SubgroupVoteKHR";
    case spv::Capability::StorageBuffer16BitAccess:
        return "StorageBuffer16BitAccess";
    case spv::Capability::StorageUniform16:
        return "StorageUniform16";
    case spv::Capability::StoragePushConstant16:
        return "StoragePushConstant16";
    case spv::Capability::StorageInputOutput16:
        return "StorageInputOutput16";
    case spv::Capability::DeviceGroup:
        return "DeviceGroup";
    case spv::Capability::MultiView:
        return "MultiView";
    case spv::Capability::VariablePointersStorageBuffer:
        return "VariablePointersStorageBuffer";
    case spv::Capability::VariablePointers:
        return "VariablePointers";
    case spv::Capability::AtomicStorageOps:
        return "AtomicStorageOps";
    case spv::Capability::SampleMaskPostDepthCoverage:
        return "SampleMaskPostDepthCoverage";
    case spv::Capability::ImageGatherBiasLodAMD:
        return "ImageGatherBiasLodAMD";
    case spv::Capability::FragmentMaskAMD:
        return "FragmentMaskAMD";
    case spv::Capability::StencilExportEXT:
        return "StencilExportEXT";
    case spv::Capability::ImageReadWriteLodAMD:
        return "ImageReadWriteLodAMD";
    case spv::Capability::SampleMaskOverrideCoverageNV:
        return "SampleMaskOverrideCoverageNV";
    case spv::Capability::GeometryShaderPassthroughNV:
        return "GeometryShaderPassthroughNV";
    case spv::Capability::ShaderViewportIndexLayerEXT:
        return "ShaderViewportIndexLayerEXT";
    case spv::Capability::ShaderViewportMaskNV:
        return "ShaderViewportMaskNV";
    case spv::Capability::ShaderStereoViewNV:
        return "ShaderStereoViewNV";
    case spv::Capability::PerViewAttributesNV:
        return "PerViewAttributesNV";
    case spv::Capability::SubgroupShuffleINTEL:
        return "SubgroupShuffleINTEL";
    case spv::Capability::SubgroupBufferBlockIOINTEL:
        return "SubgroupBufferBlockIOINTEL";
    case spv::Capability::SubgroupImageBlockIOINTEL:
        return "SubgroupImageBlockIOINTEL";
    default:
        throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid capability constant!");
    }
    throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid capability constant!");
}

static const std::set<spv::Capability> supportedCapabilites = {
    // OpenCL kernels
    spv::Capability::Kernel,
    // support for 8-component or 16-component vectors
    spv::Capability::Vector16,
    // support for async_work_group_copy, ...
    spv::Capability::Groups,
    // support for short-type
    spv::Capability::Int16,
    // support for char-type
    spv::Capability::Int8,
    // support for physical, non-logical addressing mode
    spv::Capability::Addresses,
    // support for linking functions in/out of the module
    // need to be supported, since vc4cl_* intrinsics are linked in
    spv::Capability::Linkage,
    // support for image types
    spv::Capability::ImageBasic,
    // reading/writing images
    spv::Capability::ImageReadWrite,
    // samplers from literal constants
    spv::Capability::LiteralSampler,
    // support for sampled 1D images
    spv::Capability::Sampled1D,
    // support for un-sampled 1D images
    spv::Capability::Image1D,
    // support for sampled image-buffers
    spv::Capability::SampledBuffer,
    // support for un-sampled image-buffers
    spv::Capability::ImageBuffer,
    //"generic" storage class, is never actually checked, so why not
    spv::Capability::GenericPointer,
    // support for half floating-point type only as pointer-type
    spv::Capability::Float16Buffer, spv::Capability::Int64, spv::Capability::Float64};

spv_result_t spirv2qasm::checkCapability(const spv::Capability cap)
{
    // see https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#Capability
    /*
     * see  SPIR-V OpenCL environment specification, section 6.2:
     * "An OpenCL 1.2 Embedded Profile platform is guaranteed to support, at least, the following SPIR-V capabilities:
     *   Address, Float16Buffer, Group, Int16, Int8, Kernel, Linkage, LiteralSampler, Vector16
     *  Furthermore, the following capabilities may be supported:
     *   ImageBasic, Int64"
     */

    const std::string name = getCapabilityName(cap);
    if(supportedCapabilites.find(cap) != supportedCapabilites.end())
    {
        CPPLOG_LAZY(logging::Level::DEBUG, log << "Using supported capability: " << name << logging::endl);
        return SPV_SUCCESS;
    }
    CPPLOG_LAZY(logging::Level::DEBUG, log << "Using unsupported capability: " << name << logging::endl);
    return SPV_UNSUPPORTED;
}

spv_result_t spirv2qasm::checkExtension(const std::string& extension)
{
    if(extension == "SPV_KHR_no_integer_wrap_decoration")
        // Adds support for integer wrap/nowrap decorations, similar to LLVM nsw/nuw flags
        // https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_no_integer_wrap_decoration.html
        return SPV_SUCCESS;
    // a list of all extension: https://www.khronos.org/registry/spir-v/
    return SPV_UNSUPPORTED;
}

Optional<uint32_t> spirv2qasm::getDecoration(
    const std::vector<std::pair<spv::Decoration, uint32_t>>& decorations, const spv::Decoration deco)
{
    for(const auto& pair : decorations)
    {
        if(pair.first == deco)
            return pair.second;
    }
    return Optional<uint32_t>{};
}

static ParameterDecorations toDecoration(spv::FunctionParameterAttribute attribute)
{
    switch(attribute)
    {
    case spv::FunctionParameterAttribute::NoWrite:
        return ParameterDecorations::READ_ONLY;
    case spv::FunctionParameterAttribute::Sext:
        return ParameterDecorations::SIGN_EXTEND;
    case spv::FunctionParameterAttribute::Zext:
        return ParameterDecorations::ZERO_EXTEND;
    case spv::FunctionParameterAttribute::NoAlias:
        return ParameterDecorations::RESTRICT;
    case spv::FunctionParameterAttribute::ByVal:
        return ParameterDecorations::BY_VALUE;
    default:
        return ParameterDecorations::NONE;
    }
}

void spirv2qasm::setParameterDecorations(
    Parameter& param, const std::vector<std::pair<spv::Decoration, uint32_t>>& decorations)
{
    if(auto decoration = getDecoration(decorations, spv::Decoration::FuncParamAttr))
        param.decorations =
            add_flag(param.decorations, toDecoration(static_cast<spv::FunctionParameterAttribute>(decoration.value())));
    if(auto decoration = getDecoration(decorations, spv::Decoration::MaxByteOffset))
        param.maxByteOffset = decoration.value();

    for(const auto& pair : decorations)
    {
        if(pair.first == spv::Decoration::Constant)
            param.decorations = add_flag(param.decorations, ParameterDecorations::READ_ONLY);
        else if(pair.first == spv::Decoration::Restrict)
            param.decorations = add_flag(param.decorations, ParameterDecorations::RESTRICT);
        else if(pair.first == spv::Decoration::Volatile)
            param.decorations = add_flag(param.decorations, ParameterDecorations::VOLATILE);
    }

    if(has_flag(param.decorations, ParameterDecorations::BY_VALUE))
    {
        // Same as for LLVM front-end (in BitcodeReader.cpp#parseFunction)
        // is always read-only, and the address-space initially set is __private, which we cannot have for pointer
        // Parameters
        // TODO remove, pass to consructor! Same for all other setting of values
        const_cast<PointerType*>(param.type.getPointerType())->addressSpace = AddressSpace::CONSTANT;
    }

    // TODO according to the SPIR-V specification 1.3 revision 5, 2.18.2: "The OpenCL memory model must, unless
    // otherwise proven, assume that memory object declarations might alias each other."
}

DataType spirv2qasm::getIntegerType(const uint32_t bitWidth, const uint32_t signedness)
{
    if(bitWidth > 64)
        throw CompilationError(CompilationStep::PARSER, "Unsupported bit-width for integer", std::to_string(bitWidth));
    if(bitWidth == 64)
    {
        logging::warn()
            << "64-bit operations are not supported by the VideoCore IV architecture, further compilation may fail!"
            << logging::endl;
        return TYPE_INT64;
    }
    if(bitWidth == 32)
        return TYPE_INT32;
    if(bitWidth == 16)
        return TYPE_INT16;
    if(bitWidth == 8)
        return TYPE_INT8;
    CPPLOG_LAZY(
        logging::Level::DEBUG, log << "Unrecognized integer type with " << bitWidth << " bits" << logging::endl);
    return DataType{static_cast<uint8_t>(bitWidth), 1, false};
}

AddressSpace spirv2qasm::toAddressSpace(const spv::StorageClass storageClass)
{
    switch(storageClass)
    {
    case spv::StorageClass::Generic:
        return AddressSpace::GENERIC;
    case spv::StorageClass::Private:
    case spv::StorageClass::Function:
        return AddressSpace::PRIVATE;
    case spv::StorageClass::CrossWorkgroup:
        return AddressSpace::GLOBAL;
    case spv::StorageClass::UniformConstant:
    case spv::StorageClass::Input:
        // "Visible across all functions in the current invocation. Variables declared with this storage class are
        // read-only [...]" - SPIR-V specification, section 3.7. Storage Class
        return AddressSpace::CONSTANT;
    case spv::StorageClass::Workgroup:
        return AddressSpace::LOCAL;
    default:
        logging::warn() << "Unrecognized storage-class " << static_cast<unsigned>(storageClass) << logging::endl;
    }
    // OpenCL's default address space
    return AddressSpace::PRIVATE;
}

void spirv2qasm::consumeSPIRVMessage(
    spv_message_level_t level, const char* source, const spv_position_t& position, const char* message)
{
    std::string levelText;
    switch(level)
    {
    case SPV_MSG_DEBUG:
        levelText = "Debug";
        break;
    case SPV_MSG_ERROR:
        levelText = "Error";
        break;
    case SPV_MSG_FATAL:
        levelText = "Fatal";
        break;
    case SPV_MSG_INFO:
        levelText = "Info";
        break;
    case SPV_MSG_INTERNAL_ERROR:
        levelText = "Internal Error";
        break;
    case SPV_MSG_WARNING:
        levelText = "Warning";
        break;
    }
    CPPLOG_LAZY(logging::Level::INFO,
        log << "SPIR-V Tools: " << levelText << " message in '" << source << "' at position " << position.line << ":"
            << position.column << ": " << message << logging::endl);
}

std::vector<uint32_t> spirv2qasm::readStreamOfWords(std::istream* in)
{
    std::vector<uint32_t> words;
    words.reserve(static_cast<std::size_t>(in->rdbuf()->in_avail()));
    char buffer[sizeof(uint32_t)];
    while(in->read(buffer, sizeof(uint32_t)).good())
    {
        words.push_back(*reinterpret_cast<uint32_t*>(buffer));
    }

    return words;
}

void spirv2qasm::linkSPIRVModules(const std::vector<std::istream*>& inputModules, std::ostream& output)
{
#ifndef SPIRV_FRONTEND
    throw CompilationError(CompilationStep::LINKER, "SPIRV-Tools linker is not available!");
#else
    std::vector<std::vector<uint32_t>> binaries;
    binaries.reserve(inputModules.size());
    std::transform(inputModules.begin(), inputModules.end(), std::back_inserter(binaries), readStreamOfWords);

    spvtools::LinkerOptions options;
    options.SetCreateLibrary(false);
    options.SetVerifyIds(true);
    // the VC4CL intrinsics are not provided by any input module
    options.SetAllowPartialLinkage(true);

    spvtools::Context spvContext(SPV_ENV_OPENCL_EMBEDDED_1_2);

    std::vector<uint32_t> linkedModules;
    spv_result_t result = spvtools::Link(spvContext, binaries, &linkedModules, options);

    if(result != SPV_SUCCESS)
        throw CompilationError(CompilationStep::PARSER, getErrorMessage(result));

    for(const uint32_t u : linkedModules)
    {
        output.write(reinterpret_cast<const char*>(&u), sizeof(uint32_t));
    }
    CPPLOG_LAZY(logging::Level::DEBUG,
        log << "Linked " << inputModules.size() << " modules into a single module with " << linkedModules.size()
            << " words of data." << logging::endl);
#endif
}

std::string spirv2qasm::demangleFunctionName(const std::string& name)
{
    if(name.find("_Z") != 0)
        return name;

#ifdef __GNUC__
    int status;
    char* real_name = abi::__cxa_demangle(name.data(), nullptr, nullptr, &status);
    std::string result = name;

    if(status == 0)
    {
        // if demangling is successful, output the demangled function name
        result = real_name;
        // the demangled name contains the arguments, so we need ignore them
        result = result.substr(0, result.find('('));
        CPPLOG_LAZY(
            logging::Level::DEBUG, log << "Demangled function name '" << name << "' to: " << result << logging::endl);
    }
    free(real_name);
    return result;
#else
    return name;
#endif
}
#endif /* SPIRV_HEADER */
