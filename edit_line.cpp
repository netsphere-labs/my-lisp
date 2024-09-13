
// GNU Readline は GPLv3 でライセンスされるため, GPLed なソフトウェア以外では
// 使えない. libedit-devel-3.1-45.20221030cvs.fc38.x86_64 を使う

#include "edit_line.h"
#include "my_debug.h"
#include <stdlib.h>
#include <stdio.h>
//#include <unistd.h>
#include <editline/readline.h>
#include <unicode/unistr.h>
using namespace icu;

/* @note /usr/include/readline/tilde.h ファイルで定義される:
         extern char *tilde_expand (const char *);
*/


namespace my {

EditLine::EditLine(const UnicodeString& historyFile)
{
    std::string u;
    char* v = tilde_expand( (char*) historyFile.toUTF8String(u).c_str() );
    m_historyPath = UnicodeString((const char*) v);
    free(v);

    u = "";
    m_historyPath.toUTF8String(u);
    TRACE("history file = %s\n", u.c_str() );
    read_history( u.c_str() );
}

EditLine::~EditLine()
{
    std::string u;
    write_history(m_historyPath.toUTF8String(u).c_str());
}

bool EditLine::get(const UnicodeString& prompt, UnicodeString* out)
{
    std::string u;
    char *line = readline( prompt.toUTF8String(u).c_str() );
    if (line == NULL)
        return false; // EOF かつ行が空のとき

    add_history(line); // Add input to in-memory history
    append_history(1, m_historyPath.toUTF8String(u).c_str() );

    *out = UnicodeString((const char*) line);
    free(line);

    return true;
}

} // namespace my
