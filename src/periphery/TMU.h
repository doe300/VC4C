/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_TMU_H
#define VC4C_TMU_H

#include "../Method.h"
#include "../Values.h"
#include "../asm/OpCodes.h"
#include "CacheEntry.h"

namespace vc4c
{
    namespace periphery
    {
        const Value TMU_READ_REGISTER(REG_TMU_OUT, TYPE_UNKNOWN);

        struct TMU;

        struct TMUCacheEntry : CacheEntry
        {
            TMUCacheEntry(const TMU& tmu, const Value& addr, DataType originalType);
            ~TMUCacheEntry() noexcept override;

            std::string to_string() const override;

            inline bool isReadOnly() const override
            {
                return true;
            }

            uint8_t getTMUIndex() const noexcept;

            /**
             * Returns the single instruction reading the data from memory into the TMU cache entry
             */
            intermediate::RAMAccessInstruction* getRAMReader();
            const intermediate::RAMAccessInstruction* getRAMReader() const;
            /**
             * Returns the single instruction reading the data from the TMU cache entry into a QPU register
             */
            intermediate::CacheAccessInstruction* getCacheReader();
            const intermediate::CacheAccessInstruction* getCacheReader() const;

            const unsigned index;
            const TMU& tmu;
            /**
             * Track the per-element addresses/offsets.
             *
             * This is required to be able to determine the byte/half-word offset for 8-bit/16-bit loads on reading from
             * the TMU cache.
             *
             * NOTE: Storage if this value here is okay, since it is not accessed at all before the memory access
             * lowering and therefore ignored by any optimization.
             */
            const Value addresses;
            /**
             * The number of vector elements to contain valid/useful data.
             *
             * For a number N the first N vector-elements will contain the words loaded from:
             * - base-address
             * - base-address + 1 * type-size
             * - base-address + 2 * type-size
             * - ...
             * - base-address + (N-1) * type-size
             *
             * NOTE: The other elements of the vector will have undefined contents.
             *
             * NOTE: The single addresses are truncated to multiple of 32-bit words hardware-side!
             *
             * NOTE: This usage is not tracked, so any optimization removing unread local MUST not be run yet as long as
             * this cache entry is not lowered!
             */
            Value numVectorElements;
            /**
             * The stride between consecutive 32-bit word elements being loaded.
             *
             * An element stride of less than 4 will result in multiple adjacent vector elements to contain the same
             * 32-bit word content (since the addresses are truncated to 32-bit word addresses hardware-side). Using an
             * element stride of more than 4 will result in memory being skipped and not read into any output element.
             */
            uint32_t elementStrideInBytes;
            /**
             * If this flag is is set, the addresses are assumed to be already calculated (e.g. by optimization or some
             * special case handling) and will not be overwritten on lowering the RAM access for this TMU cache entry.
             */
            bool customAddressCalculation;
        };

        /**
         * TMU
         *
         * Every QPU has access to 2 TMUs (TMU_0 and TMU_1) and can queue up to 4 requests to each of the TMUs.
         * The VC4 has 2 TMUs shared by all QPUs on a slice. Nevertheless, querying memory via a TMU does not require a
         * mutex-lock. The TMU memory-lookup is per-element, so element X will return the value from the address  given
         * by element X, write 0 per element to disable.
         */
        struct TMU
        {
            const Register s_coordinate;
            const Register t_coordinate;
            const Register r_border_color;
            const Register b_lod_bias;
            const Signaling signal;

            inline Value getAddress(DataType type) const
            {
                return Value(s_coordinate, type);
            }

            inline Value getXCoord(DataType type = TYPE_FLOAT) const
            {
                return Value(s_coordinate, type);
            }

            inline Value getYCoord(DataType type = TYPE_FLOAT) const
            {
                return Value(t_coordinate, type);
            }

            inline std::shared_ptr<TMUCacheEntry> createEntry(const Value& addresses, DataType originalType) const
            {
                return std::make_shared<TMUCacheEntry>(*this, addresses, originalType);
            }
        };

        extern const TMU TMU0;
        extern const TMU TMU1;

        /*
         * Generates the intermediate TMU memory/cache access instructions representing the given TMU data lookup.
         */
        NODISCARD InstructionWalker insertReadVectorFromTMU(
            Method& method, InstructionWalker it, const Value& dest, const Value& addr, const TMU& tmu = TMU0);

        /**
         * Lowers the intermediate TMU (cache) access instructions to the hardware instructions actually performed to
         * load the associated memory.
         *
         * This function also inserts the instructions to perform the necessary type conversions and to calculate the
         * address offsets (for non-scalar non-32-bit types).
         */
        NODISCARD InstructionWalker lowerTMURead(Method& method, InstructionWalker it);

    } /* namespace periphery */
} /* namespace vc4c */

#endif /* VC4C_TMU_H */
