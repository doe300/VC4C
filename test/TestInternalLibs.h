/*
 * Author: nomaddo
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TESTINTERNALLIBS_H
#define VC4C_TESTINTERNALLIBS_H

#include "cpptest.h"

class TestInternalLibs : public Test::Suite {
public:
    TestInternalLibs();
    ~TestInternalLibs() override;

    void testGraph();
};


#endif //VC4C_TESTINTERNALLIBS_H
