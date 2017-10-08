/*
 * Author: doe300
 *
 * See the file "LICENSE" for the full license governing this code.
 */

#ifndef PROCESSUTIL_H
#define PROCESSUTIL_H

#include <iostream>
#include <string>

namespace vc4c
{
	int runProcess(const std::string& command, std::istream* stdin = nullptr, std::ostream* stdout = nullptr, std::ostream* stderr = nullptr);

} /* namespace vc4c */

#endif /* PROCESSUTIL_H */
