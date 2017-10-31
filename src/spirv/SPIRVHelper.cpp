/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "SPIRVHelper.h"
#include "CompilationError.h"

#include "log.h"
#include "../performance.h"

#ifdef SPIRV_HEADER
#ifdef SPIRV_LINKER_HEADER
#include SPIRV_LINKER_HEADER
#endif

using namespace vc4c;
using namespace vc4c::spirv2qasm;

std::string spirv2qasm::getOpenCLMethodName(const uint32_t instructionID)
{
	switch (static_cast<OpenCLLIB::Entrypoints>(instructionID))
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
			return "fclamp";
		case OpenCLLIB::Entrypoints::FMax_common:
			return "fmax";
		case OpenCLLIB::Entrypoints::FMin_common:
			return "fmin";
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
		case OpenCLLIB::Entrypoints::Fma:
			return "fma";
		case OpenCLLIB::Entrypoints::Fmax:
			return "fmax";
		case OpenCLLIB::Entrypoints::Fmin:
			return "fmin";
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
			throw CompilationError(CompilationStep::PARSER, "Unsupported OpenCL standard-function", std::to_string(instructionID));
	}
}

std::string spirv2qasm::getErrorMessage(spv_result_t error)
{
	switch (error)
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

static std::string getCapabilityName(const SpvCapability cap)
{
	switch(cap)
	{
		case SpvCapabilityMatrix :
			return "Matrix";
		case SpvCapabilityShader :
			return "Shader";
		case SpvCapabilityGeometry :
			return "Geometry";
		case SpvCapabilityTessellation :
			return "Tessellation";
		case SpvCapabilityAddresses :
			return "Addresses";
		case SpvCapabilityLinkage :
			return "Linkage";
		case SpvCapabilityKernel :
			return "Kernel";
		case SpvCapabilityVector16 :
			return "Vector16";
		case SpvCapabilityFloat16Buffer :
			return "Float16Buffer";
		case SpvCapabilityFloat16 :
			return "Float16";
		case SpvCapabilityFloat64 :
			return "Float64";
		case SpvCapabilityInt64 :
			return "Int64";
		case SpvCapabilityInt64Atomics :
			return "Int64Atomics";
		case SpvCapabilityImageBasic :
			return "ImageBasic";
		case SpvCapabilityImageReadWrite :
			return "ImageReadWrite";
		case SpvCapabilityImageMipmap :
			return "ImageMipmap";
		case SpvCapabilityPipes :
			return "Pipes";
		case SpvCapabilityGroups :
			return "Groups";
		case SpvCapabilityDeviceEnqueue :
			return "DeviceEnqueue";
		case SpvCapabilityLiteralSampler :
			return "LiteralSampler";
		case SpvCapabilityAtomicStorage :
			return "AtomicStorage";
		case SpvCapabilityInt16 :
			return "Int16";
		case SpvCapabilityTessellationPointSize :
			return "TessellationPointSize";
		case SpvCapabilityGeometryPointSize :
			return "GeometryPointSize";
		case SpvCapabilityImageGatherExtended :
			return "ImageGatherExtended";
		case SpvCapabilityStorageImageMultisample :
			return "StorageOmageMultisample";
		case SpvCapabilityUniformBufferArrayDynamicIndexing :
			return "UniformBifferArrayDynamicIndexing";
		case SpvCapabilitySampledImageArrayDynamicIndexing :
			return "SampledImageArrayDynamicIndexing";
		case SpvCapabilityStorageBufferArrayDynamicIndexing :
			return "StorageBufferArrayDynamicIndexing";
		case SpvCapabilityStorageImageArrayDynamicIndexing :
			return "StorageImageArrayDynamicIndexing";
		case SpvCapabilityClipDistance :
			return "ClipDistance";
		case SpvCapabilityCullDistance :
			return "CullDistance";
		case SpvCapabilityImageCubeArray :
			return "ImageCubeArray";
		case SpvCapabilitySampleRateShading :
			return "SampleRateShading";
		case SpvCapabilityImageRect :
			return "ImageRect";
		case SpvCapabilitySampledRect :
			return "SampledRect";
		case SpvCapabilityGenericPointer :
			return "GenericPointer";
		case SpvCapabilityInt8 :
			return "Int8";
		case SpvCapabilityInputAttachment :
			return "InputAttachment";
		case SpvCapabilitySparseResidency :
			return "SparseResidency";
		case SpvCapabilityMinLod :
			return "MinLod";
		case SpvCapabilitySampled1D :
			return "Sampled1D";
		case SpvCapabilityImage1D :
			return "Image1D";
		case SpvCapabilitySampledCubeArray :
			return "SampledCubeArray";
		case SpvCapabilitySampledBuffer :
			return "SampledBuffer";
		case SpvCapabilityImageBuffer :
			return "ImageBuffer";
		case SpvCapabilityImageMSArray :
			return "ImageMSArray";
		case SpvCapabilityStorageImageExtendedFormats :
			return "StorageImageExtendedFormats";
		case SpvCapabilityImageQuery :
			return "ImageQuery";
		case SpvCapabilityDerivativeControl :
			return "DerivativeControl";
		case SpvCapabilityInterpolationFunction :
			return "InterpolationFunction";
		case SpvCapabilityTransformFeedback :
			return "TransformFeedback";
		case SpvCapabilityGeometryStreams :
			return "GeometryStreams";
		case SpvCapabilityStorageImageReadWithoutFormat :
			return "StorageImageReadWithoutFormat";
		case SpvCapabilityStorageImageWriteWithoutFormat :
			return "StorageImageWriteWithoutFormat";
		case SpvCapabilityMultiViewport :
			return "MultiViewport";
		case SpvCapabilitySubgroupDispatch :
			return "SubgroupDispatch";
		case SpvCapabilityNamedBarrier :
			return "NamedBarrier";
		case SpvCapabilityPipeStorage :
			return "PipeStorage";
		case SpvCapabilitySubgroupBallotKHR :
			return "SubgroupBallotKHR";
		case SpvCapabilityDrawParameters :
			return "DrawParameters";
		case SpvCapabilitySubgroupVoteKHR :
			return "SubgroupVoteKHR";
		case SpvCapabilityStorageBuffer16BitAccess :
			return "StorageBuffer16BitAccess";
		case SpvCapabilityStorageUniform16 :
			return "StorageUniform16";
		case SpvCapabilityStoragePushConstant16 :
			return "StoragePushConstant16";
		case SpvCapabilityStorageInputOutput16 :
			return "StorageInputOutput16";
		case SpvCapabilityDeviceGroup :
			return "DeviceGroup";
		case SpvCapabilityMultiView :
			return "MultiView";
		case SpvCapabilityVariablePointersStorageBuffer :
			return "VariablePointersStorageBuffer";
		case SpvCapabilityVariablePointers :
			return "VariablePointers";
		case SpvCapabilityAtomicStorageOps :
			return "AtomicStorageOps";
		case SpvCapabilitySampleMaskPostDepthCoverage :
			return "SampleMaskPostDepthCoverage";
		case SpvCapabilityImageGatherBiasLodAMD :
			return "ImageGatherBiasLodAMD";
		case SpvCapabilityFragmentMaskAMD :
			return "FragmentMaskAMD";
		case SpvCapabilityStencilExportEXT :
			return "StencilExportEXT";
		case SpvCapabilityImageReadWriteLodAMD :
			return "ImageReadWriteLodAMD";
		case SpvCapabilitySampleMaskOverrideCoverageNV :
			return "SampleMaskOverrideCoverageNV";
		case SpvCapabilityGeometryShaderPassthroughNV :
			return "GeometryShaderPassthroughNV";
		case SpvCapabilityShaderViewportIndexLayerEXT :
			return "ShaderViewportIndexLayerEXT";
		case SpvCapabilityShaderViewportMaskNV :
			return "ShaderViewportMaskNV";
		case SpvCapabilityShaderStereoViewNV :
			return "ShaderStereoViewNV";
		case SpvCapabilityPerViewAttributesNV :
			return "PerViewAttributesNV";
		case SpvCapabilitySubgroupShuffleINTEL:
			return "SubgroupShuffleINTEL";
		case SpvCapabilitySubgroupBufferBlockIOINTEL:
			return "SubgroupBufferBlockIOINTEL";
		case SpvCapabilitySubgroupImageBlockIOINTEL:
			return "SubgroupImageBlockIOINTEL";
		case SpvCapabilityMax:
			throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid capability constant!");
	}
	throw CompilationError(CompilationStep::LLVM_2_IR, "Invalid capability constant!");
}

static const std::set<SpvCapability> supportedCapabilites = {
	//OpenCL kernels
	SpvCapabilityKernel,
	//support for 8-component or 16-component vectors
	SpvCapabilityVector16,
	//support for async_work_group_copy, ...
	SpvCapabilityGroups,
	//support for short-type
	SpvCapabilityInt16,
	//support for char-type
	SpvCapabilityInt8,
	//support for physical, non-logical addressing mode
	SpvCapabilityAddresses,
	//support for linking functions in/out of the module
	//need to be supported, since vc4cl_* intrinsics are linked in
	SpvCapabilityLinkage,
	//support for image types
	SpvCapabilityImageBasic,
	//reading/writing images
	SpvCapabilityImageReadWrite,
	//samplers from literal constants
	SpvCapabilityLiteralSampler,
	//support for sampled 1D images
	SpvCapabilitySampled1D,
	//support for un-sampled 1D images
	SpvCapabilityImage1D,
	//support for sampled image-buffers
	SpvCapabilitySampledBuffer,
	//support for un-sampled image-buffers
	SpvCapabilityImageBuffer,
	//"generic" storage class, is never actually checked, so why not
	SpvCapabilityGenericPointer,
	//support for half floating-point type only as pointer-type
	SpvCapabilityFloat16Buffer
};

spv_result_t spirv2qasm::checkCapability(const SpvCapability cap)
{
	//see https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html#Capability
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
		logging::debug() << "Using supported capability: " << name << logging::endl;
		return SPV_SUCCESS;
	}
	logging::debug() << "Using unsupported capability: " << name << logging::endl;
	return SPV_UNSUPPORTED;
}

Optional<uint32_t> spirv2qasm::getDecoration(const std::vector<std::pair<SpvDecoration, uint32_t>>& decorations, const SpvDecoration deco)
{
    for(const auto& pair : decorations)
    {
        if(pair.first == deco)
            return pair.second;
    }
    return Optional<uint32_t>(false);
}

static ParameterDecorations toDecoration(SpvFunctionParameterAttribute attribute)
{
    switch(attribute)
    {
    case SpvFunctionParameterAttributeNoWrite:
        return ParameterDecorations::READ_ONLY;
    case SpvFunctionParameterAttributeSext:
        return ParameterDecorations::SIGN_EXTEND;
    case SpvFunctionParameterAttributeZext:
        return ParameterDecorations::ZERO_EXTEND;
    case SpvFunctionParameterAttributeNoAlias:
    	return ParameterDecorations::RESTRICT;
    default:
        return ParameterDecorations::NONE;
    }
}

void spirv2qasm::setParameterDecorations(Parameter& param, const std::vector<std::pair<SpvDecoration, uint32_t>>& decorations)
{
	auto decoration = getDecoration(decorations, SpvDecorationFuncParamAttr);
	if(decoration.hasValue)
		param.decorations = add_flag(param.decorations, toDecoration(static_cast<SpvFunctionParameterAttribute>(decoration.get())));
	decoration = getDecoration(decorations, SpvDecorationMaxByteOffset);
	if(decoration.hasValue)
		param.maxByteOffset = decoration.get();

	for(const auto& pair : decorations)
	{
		if(pair.first == SpvDecorationConstant)
			param.decorations = add_flag(param.decorations, ParameterDecorations::READ_ONLY);
		else if(pair.first == SpvDecorationRestrict)
			param.decorations = add_flag(param.decorations, ParameterDecorations::RESTRICT);
		else if(pair.first == SpvDecorationVolatile)
			param.decorations = add_flag(param.decorations, ParameterDecorations::VOLATILE);
	}
}

DataType spirv2qasm::getIntegerType(const uint32_t bitWidth, const uint32_t signedness)
{
    if (bitWidth > 64)
        throw CompilationError(CompilationStep::PARSER, "Unsupported bit-width for integer", std::to_string(bitWidth));
    if(bitWidth == 64)
    	return TYPE_INT64;
    if (bitWidth == 32)
        return TYPE_INT32;
    if (bitWidth == 16)
        return TYPE_INT16;
    if (bitWidth == 8)
        return TYPE_INT8;
    logging::debug() << "Unrecognized integer type with " << bitWidth << " bits" << logging::endl;
    return DataType(std::string("i") + std::to_string(bitWidth));
}

AddressSpace spirv2qasm::toAddressSpace(const SpvStorageClass storageClass)
{
	switch(storageClass)
	{
		case SpvStorageClassGeneric:
			return AddressSpace::GENERIC;
		case SpvStorageClassPrivate:
		case SpvStorageClassFunction:
			return AddressSpace::PRIVATE;
		case SpvStorageClassCrossWorkgroup:
			return AddressSpace::GLOBAL;
		case SpvStorageClassUniformConstant:
			return AddressSpace::CONSTANT;
		case SpvStorageClassWorkgroup:
			return AddressSpace::LOCAL;
		default:
			logging::warn() << "Unrecognized storage-class " << storageClass << logging::endl;
	}
	//OpenCL's default address space
	return AddressSpace::PRIVATE;
}

void spirv2qasm::consumeSPIRVMessage(spv_message_level_t level, const char* source, const spv_position_t& position, const char* message)
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
	logging::info() << "SPIR-V Tools: " << levelText << " message in '" << source << "' at position " << position.line << ":" << position.column << ": " << message <<logging::endl;
}

std::vector<uint32_t> spirv2qasm::readStreamOfWords(std::istream& in)
{
	std::vector<uint32_t> words;
	words.reserve(in.rdbuf()->in_avail());
	char buffer[sizeof (uint32_t)];
	while (in.read(buffer, sizeof (uint32_t)).good()) {
		words.push_back(*reinterpret_cast<uint32_t*>(buffer));
	}

	return words;
}

void spirv2qasm::linkSPIRVModules(const std::vector<std::istream*>& inputModules, std::ostream& output)
{
#ifndef SPIRV_LINKER_HEADER
	throw CompilationError(CompilationStep::LINKER, "SPIRV-Tools linker is not available!");
#else
	std::vector<std::vector<uint32_t>> binaries;
	binaries.reserve(inputModules.size());
	for(std::istream* is : inputModules)
	{
		binaries.push_back(readStreamOfWords(*is));
	}

	spvtools::LinkerOptions options;
	options.SetCreateLibrary(false);

	spvtools::Linker linker(SPV_ENV_OPENCL_2_1);
	linker.SetMessageConsumer(consumeSPIRVMessage);

	std::vector<uint32_t> linkedModules;
	//TODO this seems only to work if all imported symbols are exported by one of the modules, even the intrinsic symbols!
	spv_result_t result = linker.Link(binaries, linkedModules, options);

	if(result != SPV_SUCCESS)
		throw CompilationError(CompilationStep::PARSER, getErrorMessage(result));

	for(const uint32_t u : linkedModules)
	{
		output.write(reinterpret_cast<const char*>(&u), sizeof(uint32_t));
	}
	logging::debug() << "Linked " << inputModules.size() << " modules into a single module with " << linkedModules.size() << " words of data." << logging::endl;
#endif
}

#endif /* SPIRV_HEADER */
