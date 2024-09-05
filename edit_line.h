
#ifndef INCLUDE_READLINE_H
#define INCLUDE_READLINE_H

#include <unicode/unistr.h>

namespace my {

class EditLine
{
public:
    EditLine(const icu::UnicodeString& historyFile);
    ~EditLine();

    bool get(const icu::UnicodeString& prompt, icu::UnicodeString* line);

private:
    icu::UnicodeString m_historyPath;
};

} // namespace my

#endif // INCLUDE_READLINE_H
