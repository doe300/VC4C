/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_REGISTER_LOWERED_MEMORY_H
#define VC4C_REGISTER_LOWERED_MEMORY_H

#include "../InstructionWalker.h"
#include "CacheEntry.h"

namespace vc4c
{
    class Method;

    namespace periphery
    {
        /**
         * A cache entry for "memory" accesses to memory areas lowered to registers (e.g. stack variables)
         */
        struct RegisterCacheEntry : public CacheEntry
        {
            RegisterCacheEntry(const Value& cacheRegister, const Value& addr, DataType valueType);
            ~RegisterCacheEntry() noexcept override;

            std::string to_string() const override;

            inline bool isReadOnly() const override
            {
                return false;
            }

            Value precalculateOffset() const;

            /**
             * The lowered register containing the accessed cached data
             *
             * NOTE: This usage is not tracked, so any optimization removing unread local MUST not be run yet as long as
             * this cache entry is not lowered!
             */
            const Value loweredRegister;
            /**
             * The offset in bytes of the first element to be inserted into/extracted from the lowered register from the
             * base "address" of the register
             *
             * NOTE: This usage is not tracked, so any optimization removing unread local MUST not be run yet as long as
             * this cache entry is not lowered!
             */
            const Value byteOffset;
            /**
             * The original type for the data inserted into/extracted from the lowered register value.
             *
             * We need to track this, since in between creation of this cache entry and actual lowering, the data could
             * be modified and its type changed (e.g. a literal i32 could be converted to "i8 0").
             *
             * Also this value determines the extracted element stride as e.g. required for reading 64-bit values.
             */
            const DataType valueType;
        };

        /*
         * Generates the intermediate register cache access instructions representing the given lowered register write.
         */
        NODISCARD InstructionWalker insertWriteLoweredRegister(Method& method, InstructionWalker it, const Value& src,
            const Value& addressOffset, const Value& loweredRegister);
        NODISCARD InstructionWalker insertReadLoweredRegister(Method& method, InstructionWalker it, const Value& dest,
            const Value& addressOffset, const Value& loweredRegister);

        /**
         * Lowers the intermediate lowered register (cache) access instructions to the hardware instructions performing
         * the element insertion/extraction and type conversions.
         */
        NODISCARD InstructionWalker lowerRegisterAccess(Method& method, InstructionWalker it);

    } /* namespace periphery */
} /* namespace vc4c */

#endif /* VC4C_REGISTER_LOWERED_MEMORY_H */
