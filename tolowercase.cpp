#include <locale>

#include "runtime/alloc.h"
#include "runtime/header.h"

extern "C" {

#define KCHAR char
  string * hook_STRING_tolowercase(const string * input) {
    const std::locale default_locale;
    const size_t length = len(input);
    auto ret = static_cast<string *>(koreAllocToken(sizeof(string) + length * sizeof(KCHAR)));
    set_len(ret, length);
    for (size_t i = 0; i < length; ++i) {
        ret->data[i] = std::tolower(input->data[i], default_locale); // http://www.cplusplus.com/reference/cctype/tolower/
    }
    return ret;
  }
}
