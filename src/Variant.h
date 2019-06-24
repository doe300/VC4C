/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef VC4C_VARIANT
#define VC4C_VARIANT

#if __has_include(<variant>) && defined(__cpp_lib_variant) && __cpp_lib_variant >= 201603
#include <variant>
namespace vc4
{
    template <typename... Types>
    using Variant = std::variant<Types...>;
    namespace VariantNamespace = std;
} // namespace vc4

#elif __has_include(<tr1/variant>)
#include <tr1/variant>
namespace vc4c
{
    template <typename... Types>
    using Variant = std::tr1::variant<Types...>;
    namespace VariantNamespace = std::tr1;
} // namespace vc4c
#elif __has_include("mpark/variant.hpp")
// mpark/variant has too many warnings we cannot do anything about
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wweak-vtables"
#include "mpark/variant.hpp"
#pragma GCC diagnostic pop

namespace vc4c
{
    template <typename... Types>
    using Variant = mpark::variant<Types...>;
    namespace VariantNamespace = mpark;
} // namespace vc4c

#else
#error "No supported variant implementation found!"
#endif

#endif /* VC4C_VARIANT */
