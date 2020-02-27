/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "GlobalValues.h"

#include "SIMDVector.h"
#include "log.h"

#include <algorithm>

using namespace vc4c;

bool CompoundConstant::isAllSame() const
{
    if(getScalar())
        return true;

    if(auto entries = getCompound())
    {
        if(entries->size() < 2)
            return true;
        auto firstEntry = (*entries)[0].getScalar();
        return firstEntry && std::all_of(entries->begin(), entries->end(), [&](const CompoundConstant& entry) -> bool {
            // if items are UNDEFINED, ignore them, since maybe the remaining items all have the same value
            return entry.isUndefined() || entry.getScalar() == *firstEntry;
        });
    }
    // undefined
    return true;
}

bool CompoundConstant::isUndefined() const
{
    if(auto lit = getScalar())
    {
        return lit && lit->isUndefined();
    }
    if(auto elements = getCompound())
    {
        for(const auto& elem : *elements)
        {
            if(!elem.isUndefined())
                return false;
        }
    }

    return true;
}

bool CompoundConstant::isZeroInitializer() const
{
    if(auto lit = getScalar())
    {
        return lit->unsignedInt() == 0;
    }
    if(auto container = getCompound())
    {
        return std::all_of(container->begin(), container->end(),
            [](const CompoundConstant& elem) -> bool { return elem.isZeroInitializer(); });
    }
    // undefined
    return true;
}

Optional<Literal> CompoundConstant::getScalar() const noexcept
{
    if(auto lit = VariantNamespace::get_if<Literal>(&elements))
    {
        return *lit;
    }
    return {};
}

Optional<std::vector<CompoundConstant>> CompoundConstant::getCompound() const
{
    if(auto vec = VariantNamespace::get_if<std::vector<CompoundConstant>>(&elements))
    {
        return *vec;
    }
    return {};
}

Optional<Value> CompoundConstant::toValue() const
{
    if(auto lit = getScalar())
    {
        return Value(*lit, type.getElementType());
    }
    if(auto container = getCompound())
    {
        if(container->size() <= NATIVE_VECTOR_SIZE &&
            std::all_of(container->begin(), container->end(),
                [](const CompoundConstant& elem) -> bool { return elem.getScalar() || elem.isUndefined(); }))
        {
            SIMDVector vector;
            for(unsigned i = 0; i < std::min(container->size(), NATIVE_VECTOR_SIZE); ++i)
            {
                if(auto lit = (*container)[i].getScalar())
                    vector[i] = *lit;
            }
            // Since this should be called only very rarely (and we don't have access to the module), we use the global
            // vector store
            return GLOBAL_VECTOR_HOLDER.storeVector(std::move(vector), type);
        }
    }
    return NO_VALUE;
}

LCOV_EXCL_START
std::string CompoundConstant::to_string(bool withContent) const
{
    const std::string typeName = (type.isUnknown() ? "unknown" : type.to_string()) + ' ';
    if(auto lit = getScalar())
        return typeName + lit->to_string();
    if(auto container = getCompound())
    {
        if(withContent)
        {
            std::string tmp;
            const std::string pre = type.isVectorType() ? "<" : type.getArrayType() ? "[" : "{";
            const std::string post = type.isVectorType() ? ">" : type.getArrayType() ? "]" : "}";
            for(const auto& element : *container)
                tmp.append(element.to_string(withContent)).append(", ");
            return typeName + pre + tmp.substr(0, tmp.length() - 2) + post;
        }
        if(isZeroInitializer())
            return typeName + "zerointializer";
        return typeName + std::string("container with ") + std::to_string(container->size()) + " elements";
    }
    if(isUndefined())
        return typeName + "undefined";
    throw CompilationError(CompilationStep::GENERAL, "Unhandled compound constant type!");
}
LCOV_EXCL_STOP

Global::Global(const std::string& name, DataType globalType, CompoundConstant&& value, bool isConstant) :
    Local(globalType, name), initialValue(std::move(value)), isConstant(isConstant)
{
    if(!globalType.getPointerType())
        throw CompilationError(CompilationStep::GENERAL, "Global value needs to have a pointer type", to_string());
    if(isConstant != (globalType.getPointerType()->addressSpace == AddressSpace::CONSTANT))
        CPPLOG_LAZY(logging::Level::WARNING,
            log << "Constness attributes of global and pointer to global do not match" << to_string() << logging::endl);
}

LCOV_EXCL_START
std::string Global::to_string(bool withContent) const
{
    return Local::to_string(false) + ": " + initialValue.to_string(withContent) + (isConstant ? " (const)" : "");
}
LCOV_EXCL_STOP

bool Global::residesInMemory() const
{
    return true;
}

Local::RAIILock Global::getUsersLock() const
{
    usersLock.lock();
    return RAIILock([&]() { usersLock.unlock(); });
}
