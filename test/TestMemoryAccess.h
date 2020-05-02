/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_MEMORY_ACCESS_H
#define VC4C_TEST_MEMORY_ACCESS_H

#include "TestEmulator.h"

namespace vc4c
{
    namespace tools
    {
        struct EmulationResult;
    } // namespace tools
} // namespace vc4c

class TestMemoryAccess : public TestEmulator
{
public:
    TestMemoryAccess(const vc4c::Configuration& config = {});
    ~TestMemoryAccess() override;

    void testPrivateStorage();
    void testLocalStorage();
    void testVectorAssembly();
    void testConstantStorage();
    void testRegisterStorage();

    void testVPMWrites();
    void testVPMReads();

    // general vload/vstore tests are in TestVectorFunctions, this is to test optimizations (lowering into register/VPM)
    void testVectorLoadStoreCharPrivate();
    void testVectorLoadStoreCharLocal();
    void testVectorLoadStoreCharGlobal();
    void testVectorLoadStoreShortPrivate();
    void testVectorLoadStoreShortLocal();
    void testVectorLoadStoreShortGlobal();
    void testVectorLoadStoreIntPrivate();
    void testVectorLoadStoreIntLocal();
    void testVectorLoadStoreIntGlobal();

    // test access with vload/vstore of lowered memory areas with vector-type and element-type offsets
    void testVectorLoadStorePrivateRegister();
    void testVectorLoadStorePrivateVPMFull();
    void testVectorLoadStorePrivateVPMPartial();
    // TODO
    // void testVectorLoadStoreLocalVPMFull();
    // void testVectorLoadStoreLocalVPMPartial();
    void testVectorLoadStoreLocalParameter();
    void testVectorLoadStoreGlobalParameter();

    void testDynamicCopyRegisterToRegister();
    void testDynamicCopyRegisterToVPM();
    void testDynamicCopyRegisterToRAM();
    void testDynamicCopyVPMToRegister();
    void testDynamicCopyVPMToVPM();
    void testDynamicCopyVPMToRAM();
    void testDynamicCopyRAMToRegister();
    void testDynamicCopyRAMToVPM();
    void testDynamicCopyRAMToRAM();

    void testDynamicFillRegister();
    void testDynamicFillVPM();
    void testDynamicFillRAM();

    void testCopy64BitRAM();
    void testLoadStore64BitRAM();
    void testRead64BitLowerWordFromRAM();
    void testRead64BitUpperWordFromRAM();
    void testReadWrite64BitLowerWordFromRAM();
    void testReadWrite64BitUpperWordFromRAM();
    void testWrite64BitToRAM();

    void testWriteSelectParameter();
    void testReadSelectParameter();
    void testReadWriteSelectParameter();
    void testCopySelectParameter();
    void testWritePhiParameter();
    void testReadPhiParameter();
    void testReadWritePhiParameter();
    void testCopyPhiParameter();
    // TODO void testWriteSelectParameterOrLocal();
    void testReadSelectParameterOrLocal();
    // TODO void testWriteSelectRegister();
    void testReadSelectRegister();
    // TODO void testReadWriteSelectRegister();

private:
    void onMismatch(const std::string& expected, const std::string& result);

    void emulateKernel(std::istream& code, const std::string& kernelName, unsigned numItems,
        std::unique_ptr<vc4c::tools::EmulationResult>& result, const std::vector<std::vector<unsigned>>& args);
};

#endif /* VC4C_TEST_MEMORY_ACCESS_H */
