/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TEST_MEMORY_ACCESS_H
#define VC4C_TEST_MEMORY_ACCESS_H

#include "TestEmulator.h"

class TestMemoryAccess : public TestEmulator
{
public:
    TestMemoryAccess(const vc4c::Configuration& config = {});

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

private:
    void onMismatch(const std::string& expected, const std::string& result);
};

#endif /* VC4C_TEST_MEMORY_ACCESS_H */
