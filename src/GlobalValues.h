/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef GLOBAL_VALUES_H
#define GLOBAL_VALUES_H

#include "Locals.h"
#include "Values.h"
#include "Variant.h"

#include <vector>

namespace vc4c
{
    /*
     * Content type for constant values containing several values (e.g. struct- or array- constants)
     *
     * NOTE: This can also contain containers of non-scalar values (e.g. array of vectors)
     */
    struct CompoundConstant
    {
    public:
        explicit CompoundConstant(DataType type, Literal lit) : type(type), elements(lit) {}
        explicit CompoundConstant(DataType type, std::size_t size) :
            type(type), elements(std::vector<CompoundConstant>{})
        {
            VariantNamespace::get<std::vector<CompoundConstant>>(elements).reserve(size);
        }
        CompoundConstant(DataType type, std::vector<CompoundConstant>&& values) :
            type(type), elements(std::move(values))
        {
        }

        /*
         * Determines whether all elements of this container have the same value
         */
        bool isAllSame() const;

        /*
         * Returns whether all elements contained are undefined
         */
        bool isUndefined() const;
        /*
         * Whether this object is a constant (scalar or composite) and has the literal-value zero in all its elements
         */
        bool isZeroInitializer() const;

        Optional<Literal> getScalar() const noexcept;
        Optional<std::vector<CompoundConstant>> getCompound() const;

        /*
         * Converts this compound constant to a Value.
         * The constant can be converted, if:
         * - it is a scalar
         * - it is a vector (of scalars)
         * - it is an array of scalars with at most 16 elements
         */
        Optional<Value> toValue() const;

        std::string to_string(bool withContent = false) const;

        DataType type;

    private:
        Variant<std::vector<CompoundConstant>, Literal> elements;
    };

    /*
     * Global data, can be accessed by all kernel-functions in a module and is consistent across kernel-invocations.
     * The global data segment is part of the module binary code and contains the initial values for all global data.
     */
    struct Global final : public Local
    {
        Global(const std::string& name, DataType globalType, CompoundConstant&& initialValue, bool isConstant);
        Global(Global&&) = default;
        ~Global() override = default;

        std::string to_string(bool withContent = false) const override;

        /*
         * Returns true, since global data resides in memory
         */
        bool residesInMemory() const override;

        /*
         * The initial value, usually defaults to a zero-initializer
         */
        CompoundConstant initialValue;

        /*
         * Whether this global value is a constant
         */
        const bool isConstant;
    };
} /* namespace vc4c */

#endif /* GLOBAL_VALUES_H */
