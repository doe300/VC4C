/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef SRC_MODULE_H_
#define SRC_MODULE_H_

#include "GlobalValues.h"
#include "Method.h"
#include "SIMDVector.h"
#include "performance.h"

namespace vc4c
{
    /*
     * A module represents a compilation unit (e.g. a compilation of one source file).
     *
     * The module-class manages shared data, like globals and contains the list of methods
     */
    class Module : private NonCopyable, public TypeHolder, public SIMDVectorHolder
    {
        using MethodList = std::vector<std::unique_ptr<Method>>;

    public:
        explicit Module(const Configuration& compilationConfig);
        Module(const Module&) = delete;
        Module(Module&&) = delete;
        ~Module() = default;

        Module& operator=(const Module&) = delete;
        Module& operator=(Module&&) = delete;

        /*
         * The global data within this module
         */
        StableList<Global> globalData;
        /*
         * The module's methods
         */
        MethodList methods;
        /*
         * Additional function name aliases.
         *
         * E.g. the inlining will check this map and if the called function name is a key in this map, it will try to
         * find a function with the mapped value as name to be inlined.
         */
        FastMap<std::string, std::string> functionAliases;

        const Configuration& compilationConfig;

        inline MethodList::iterator begin()
        {
            return methods.begin();
        }

        inline MethodList::const_iterator begin() const
        {
            return methods.begin();
        }

        inline MethodList::iterator end()
        {
            return methods.end();
        }

        inline MethodList::const_iterator end() const
        {
            return methods.end();
        }

        /*
         * Returns the methods marked as OpenCL kernels
         */
        std::vector<Method*> getKernels();
        /*
         * Calculates the offset (in bytes) from the start of the global-data segment for the given local.
         *
         * If the local is a Global, the correctly aligned position of this global is returned,
         * otherwise the complete size of the global-data segment (aligned to 8 Byte) is returned.
         */
        Optional<unsigned int> getGlobalDataOffset(const Local* local) const;

        /*
         * Looks for a global with the given name and returns it.
         *
         * Returns nullptr otherwise
         */
        const Global* findGlobal(const std::string& name) const;

        /**
         * Removes all functions which are not marked as kernels to free up some memory
         */
        void dropNonKernels();
    };
} // namespace vc4c

#endif /* SRC_MODULE_H_ */
