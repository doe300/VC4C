/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Locals.h"

#include "intermediate/IntermediateInstruction.h"

using namespace vc4c;

Local::Local(DataType type, const std::string& name) : type(type), name(name), reference(nullptr, ANY_ELEMENT) {}

bool Local::operator<(const Local& other) const
{
    return name < other.name;
}

bool Local::operator==(const Local& other) const
{
    return this == &other || name == other.name;
}

Value Local::createReference(int index) const
{
    if(index != WHOLE_OBJECT)
    {
        return Value(const_cast<Local*>(this), type.getElementType(index));
    }
    return Value(const_cast<Local*>(this), type);
}

const SortedMap<const LocalUser*, LocalUse>& Local::getUsers() const
{
    return users;
}

FastSet<const LocalUser*> Local::getUsers(const LocalUse::Type type) const
{
    FastSet<const LocalUser*> users;
    for(const auto& pair : this->users)
    {
        if((has_flag(type, LocalUse::Type::READER) && pair.second.readsLocal()) ||
            (has_flag(type, LocalUse::Type::WRITER) && pair.second.writesLocal()))
            users.insert(pair.first);
    }
    return users;
}

void Local::forUsers(const LocalUse::Type type, const std::function<void(const LocalUser*)>& consumer) const
{
    for(const auto& pair : this->users)
    {
        if((has_flag(type, LocalUse::Type::READER) && pair.second.readsLocal()) ||
            (has_flag(type, LocalUse::Type::WRITER) && pair.second.writesLocal()))
            consumer(pair.first);
    }
}

void Local::removeUser(const LocalUser& user, const LocalUse::Type type)
{
    if(type == LocalUse::Type::BOTH)
    {
        // if we remove the user completely, ignore if it was a user
        users.erase(&user);
        return;
    }
    auto it = users.find(&user);
    if(it == users.end())
        throw CompilationError(
            CompilationStep::GENERAL, "Trying to remove a not registered user for a local", user.to_string());
    LocalUse& use = it->second;
    if(type == LocalUse::Type::READER)
        --use.numReads;
    else if(type == LocalUse::Type::WRITER)
        --use.numWrites;
    if(!use.readsLocal() && !use.writesLocal())
        users.erase(&user);
}

void Local::addUser(const LocalUser& user, const LocalUse::Type type)
{
    auto it = users.find(&user);
    if(it == users.end())
        it = users.emplace(&user, LocalUse()).first;
    LocalUse& use = it->second;
    if(has_flag(type, LocalUse::Type::READER))
        ++use.numReads;
    if(has_flag(type, LocalUse::Type::WRITER))
        ++use.numWrites;
}

const LocalUser* Local::getSingleWriter() const
{
    const LocalUser* writer = nullptr;
    for(const auto& pair : this->users)
    {
        if(pair.second.writesLocal())
        {
            if(writer != nullptr)
                // multiple writers
                return nullptr;
            writer = pair.first;
        }
    }
    return writer;
}

LCOV_EXCL_START
std::string Local::to_string(bool withContent) const
{
    std::string content;
    if(withContent && reference.first != nullptr)
    {
        // FIXME very often the Local referenced here is already freed by Method#cleanLocals(). The references need to
        // be updated!
        content = std::string(" (ref ") + reference.first->to_string(false) +
            (reference.second == ANY_ELEMENT ? std::string("") :
                                               (std::string(" at ") + std::to_string(reference.second))) +
            ")";
    }
    return (type.to_string() + " ") + name + content;
}
LCOV_EXCL_STOP

bool Local::residesInMemory() const
{
    return false;
}

const Local* Local::getBase(bool includeOffsets) const
{
    if(reference.first != nullptr && (reference.second == 0 || includeOffsets))
    {
        return reference.first->getBase(includeOffsets);
    }
    return this;
}

Parameter::Parameter(const std::string& name, DataType type, const ParameterDecorations decorations) :
    Local(type, name), decorations(decorations)
{
}

LCOV_EXCL_START
std::string vc4c::toString(ParameterDecorations deco)
{
    std::string res;
    if(has_flag(deco, ParameterDecorations::BY_VALUE))
        res.append("byval ");
    if(has_flag(deco, ParameterDecorations::INPUT))
        res.append("in ");
    if(has_flag(deco, ParameterDecorations::OUTPUT))
        res.append("out ");
    if(has_flag(deco, ParameterDecorations::READ_ONLY))
        res.append("const ");
    if(has_flag(deco, ParameterDecorations::RESTRICT))
        res.append("restrict ");
    if(has_flag(deco, ParameterDecorations::SIGN_EXTEND))
        res.append("sext ");
    if(has_flag(deco, ParameterDecorations::VOLATILE))
        res.append("volatile ");
    if(has_flag(deco, ParameterDecorations::ZERO_EXTEND))
        res.append("zext ");
    return res.substr(0, res.empty() ? 0 : res.size() - 1);
}

std::string Parameter::to_string(bool withContent) const
{
    auto tmp = Local::to_string(withContent);
    if(withContent)
    {
        std::vector<std::string> extras;
        if(!parameterName.empty())
            extras.emplace_back("name: " + parameterName);
        if(!origTypeName.empty())
            extras.emplace_back("type: " + origTypeName);
        if(maxByteOffset != SIZE_MAX)
            extras.emplace_back("used bytes: " + std::to_string(maxByteOffset));
        if(decorations != ParameterDecorations::NONE)
            extras.emplace_back(toString(decorations));

        if(!extras.empty())
            tmp.append(" (").append(vc4c::to_string<std::string>(extras)).append(")");
    }
    return tmp;
}
LCOV_EXCL_STOP

bool Parameter::isInputParameter() const
{
    return has_flag(decorations, ParameterDecorations::INPUT);
}

bool Parameter::isOutputParameter() const
{
    return has_flag(decorations, ParameterDecorations::OUTPUT);
}

StackAllocation::StackAllocation(const std::string& name, DataType type, std::size_t size, std::size_t alignment) :
    Local(type, name), offset(0), alignment(alignment == 0 ? 1 : alignment),
    size(size > 0 ? size : type.getElementType().getInMemoryWidth())
{
    if(!type.getPointerType())
        throw CompilationError(CompilationStep::GENERAL, "Stack allocation needs to have a pointer type!", to_string());
    if(type.getPointerType()->addressSpace != AddressSpace::PRIVATE)
        throw CompilationError(
            CompilationStep::GENERAL, "Stack allocations must have private address space", to_string());
}

bool StackAllocation::residesInMemory() const
{
    return true;
}

bool order_by_alignment_and_name::operator()(const StackAllocation& sa1, const StackAllocation& sa2) const
{
    if(sa1.alignment > sa2.alignment)
        return true;
    return sa1 < sa2;
}
