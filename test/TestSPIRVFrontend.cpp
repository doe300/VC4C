/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "TestSPIRVFrontend.h"

#include "spirv/SPIRVHelper.h"
#ifdef SPIRV_HEADER
#include SPIRV_PARSER_HEADER

using namespace vc4c::spirv2qasm;
#endif

TestSPIRVFrontend::TestSPIRVFrontend()
{
	TEST_ADD(TestSPIRVFrontend::testCapabilitiesSupport);
}

TestSPIRVFrontend::~TestSPIRVFrontend()
{
	//out-of-line virtual destructor
}

void TestSPIRVFrontend::testCapabilitiesSupport()
{
#ifdef SPIRV_HEADER
	/*
	 * see  SPIR-V OpenCL environment specification, section 6.2:
	 * "An OpenCL 1.2 Embedded Profile platform is guaranteed to support, at least, the following SPIR-V capabilities:
	 *   Address, Float16Buffer, Group, Int16, Int8, Kernel, Linkage, LiteralSampler, Vector16
	 *  Furthermore, the following capabilities may be supported:
	 *   ImageBasic, Int64"
	 */
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityAddresses));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityFloat16Buffer));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityGroups));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityInt16));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityInt8));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityKernel));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityLinkage));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityLiteralSampler));
	TEST_ASSERT_EQUALS(SPV_SUCCESS, checkCapability(SpvCapability::SpvCapabilityVector16));
#endif
}
