/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Module.h"

using namespace vc4c;

Module::Module(const Configuration& compilationConfig) : compilationConfig(compilationConfig) {}

std::vector<Method*> Module::getKernels()
{
    std::vector<Method*> kernels;
    for(auto& method : methods)
        if(method->isKernel)
            kernels.push_back(method.get());
    return kernels;
}

Optional<unsigned int> Module::getGlobalDataOffset(const Local* local) const
{
    if(local != nullptr && !local->is<Global>())
        return {};
    unsigned int offset = 0;
    for(const Global& global : globalData)
    {
        const unsigned alignment = global.type.getPointerType()->getAlignment();
        if(offset % alignment != 0)
        {
            offset += alignment - (offset % alignment);
        }
        if(local == &global)
        {
            return offset;
        }
        offset += global.initialValue.type.getInMemoryWidth();
    }
    if(local == nullptr)
        return offset;
    return {};
}

const Global* Module::findGlobal(const std::string& name) const
{
    for(const Global& global : globalData)
    {
        if(global.name == name)
            return &global;
    }
    return nullptr;
}

void Module::dropNonKernels()
{
    auto it = std::remove_if(
        methods.begin(), methods.end(), [](const auto& method) -> bool { return !method || !method->isKernel; });
    methods.erase(it, methods.end());
}
