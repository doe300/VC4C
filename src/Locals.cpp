/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#include "Locals.h"

#include "intermediate/IntermediateInstruction.h"

using namespace vc4c;

LocalData::~LocalData() noexcept = default;

std::string LocalData::to_string() const
{
    return "";
}

Local::Local(DataType type, const std::string& name) : type(type), name(name) {}

bool Local::operator<(const Local& other) const noexcept
{
    return name < other.name;
}

bool Local::operator==(const Local& other) const noexcept
{
    return this == &other || name == other.name;
}

Value Local::createReference() const
{
    return Value(const_cast<Local*>(this), type);
}

const tools::SmallSortedPointerMap<const LocalUser*, LocalUse>& Local::getUsers() const
{
    return users;
}

FastSet<const LocalUser*> Local::getUsers(const LocalUse::Type type) const
{
    auto lock = getUsersLock();
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
    auto lock = getUsersLock();
    for(const auto& pair : this->users)
    {
        if((has_flag(type, LocalUse::Type::READER) && pair.second.readsLocal()) ||
            (has_flag(type, LocalUse::Type::WRITER) && pair.second.writesLocal()))
            consumer(pair.first);
    }
}

bool Local::allUsers(LocalUse::Type type, const std::function<bool(const LocalUser*)>& consumer) const
{
    auto lock = getUsersLock();
    for(const auto& pair : this->users)
    {
        if((has_flag(type, LocalUse::Type::READER) && pair.second.readsLocal()) ||
            (has_flag(type, LocalUse::Type::WRITER) && pair.second.writesLocal()))
        {
            if(!consumer(pair.first))
                return false;
        }
    }
    return true;
}

std::size_t Local::countUsers(LocalUse::Type type) const
{
    std::size_t count = 0;
    forUsers(type, [&](const LocalUser*) { ++count; });
    return count;
}

void Local::removeUser(const LocalUser& user, const LocalUse::Type type)
{
    auto lock = getUsersLock();
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
    auto lock = getUsersLock();
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
    auto lock = getUsersLock();
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
    return (type.to_string() + " ") + name + (withContent && data ? data->to_string() : "");
}
LCOV_EXCL_STOP

bool Local::residesInMemory() const noexcept
{
    return false;
}

bool Local::residesInConstantMemory() const noexcept
{
    return false;
}

const Local* Local::getBase(bool includeOffsets) const
{
    if(auto data = get<ReferenceData>())
    {
        if(data->offset == 0 || includeOffsets)
            return data->base->getBase(includeOffsets);
    }
    return this;
}

bool Local::isMarker() const noexcept
{
    return false;
}

std::unique_lock<std::mutex> Local::getUsersLock() const
{
    return std::unique_lock<std::mutex>{/* no mutex owned*/};
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
        if(isLowered)
            extras.emplace_back("lowered");

        if(!extras.empty())
            tmp.append(" (").append(vc4c::to_string<std::string>(extras)).append(")");
    }
    return tmp;
}
LCOV_EXCL_STOP

bool Parameter::residesInMemory() const noexcept
{
    return type.getPointerType() || type.getArrayType() || type.getStructType();
}

bool Parameter::residesInConstantMemory() const noexcept
{
    return residesInMemory() && has_flag(decorations, ParameterDecorations::READ_ONLY);
}

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

LCOV_EXCL_START
std::string StackAllocation::to_string(bool withContent) const
{
    std::string suffix = "";
    if(withContent)
        suffix = " (size: " + std::to_string(size) + ", align: " + std::to_string(alignment) +
            ", offset: " + std::to_string(offset) + ")";
    return Local::to_string(withContent) + suffix;
}
LCOV_EXCL_STOP

bool StackAllocation::residesInMemory() const noexcept
{
    return true;
}

bool order_by_alignment_and_name::operator()(const StackAllocation& sa1, const StackAllocation& sa2) const
{
    if(sa1.alignment > sa2.alignment)
        return true;
    return sa1.name < sa2.name;
}

BuiltinLocal::BuiltinLocal(const std::string& name, DataType dataType, Type builtinType) :
    Local(dataType, name), builtinType(builtinType)
{
}

BuiltinLocal::~BuiltinLocal() noexcept = default;

bool BuiltinLocal::isWorkGroupUniform() const
{
    switch(builtinType)
    {
    case Type::GLOBAL_DATA_ADDRESS:
    case Type::GLOBAL_OFFSET_X:
    case Type::GLOBAL_OFFSET_Y:
    case Type::GLOBAL_OFFSET_Z:
    case Type::GROUP_ID_X:
    case Type::GROUP_ID_Y:
    case Type::GROUP_ID_Z:
    case Type::GROUP_IDS:
    case Type::LOCAL_SIZES:
    case Type::MAX_GROUP_ID_X:
    case Type::MAX_GROUP_ID_Y:
    case Type::MAX_GROUP_ID_Z:
    case Type::NUM_GROUPS_X:
    case Type::NUM_GROUPS_Y:
    case Type::NUM_GROUPS_Z:
    case Type::UNIFORM_ADDRESS:
    case Type::WORK_DIMENSIONS:
        return true;
    case Type::LOCAL_IDS:
    case Type::NUM_ENTRIES:
        return false;
    }
    throw CompilationError(
        CompilationStep::GENERAL, "Unhandled built-in type", std::to_string(static_cast<unsigned>(builtinType)));
}

MultiRegisterData::MultiRegisterData(const Local* lowerPart, const Local* upperPart) :
    lower(lowerPart), upper(upperPart)
{
}

MultiRegisterData::~MultiRegisterData() noexcept = default;

LCOV_EXCL_START
std::string MultiRegisterData::to_string() const
{
    return " (" + lower->name + ", " + upper->name + ")";
}
LCOV_EXCL_STOP

ReferenceData::ReferenceData(const Local& ref, int index) : base(&ref), offset(index) {}

ReferenceData::~ReferenceData() noexcept = default;

LCOV_EXCL_START
std::string ReferenceData::to_string() const
{
    return std::string(" (ref ") + base->to_string(false) +
        (offset == ANY_ELEMENT ? std::string("") : (std::string(" at ") + std::to_string(offset))) + ")";
}
LCOV_EXCL_STOP
