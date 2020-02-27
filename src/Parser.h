/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PARSER_H
#define PARSER_H

#include "Module.h"

namespace vc4c
{
    /*
     * Base class for any front-end implementation converting an input to a module in internal representation
     */
    class Parser
    {
    public:
        virtual ~Parser() noexcept;
        /*
         * Parses the currently set input and populates the module given with the globals/functions extracted from the
         * input.
         */
        virtual void parse(Module& module) = 0;
    };
} // namespace vc4c

#endif /* PARSER_H */
