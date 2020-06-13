#include "runtime/header.h"

extern "C" {
    char hex_value(const char c) {
        if ('0' <= c && c <= '9') {
            return c - '0';
        } else if ('a' <= c && c <= 'f') {
            return c - 'a' + 10;
        } else if ('A' <= c && c <= 'F') {
            return c - 'A' + 10;
        } else {
            throw std::invalid_argument("Hex token contains non-hex value");
        }
    }

    string * hook_BYTES_hexstring2bytes(const string * input) {
        if (layout(input) != 0) {
            throw std::invalid_argument("hexstring2bytes not passed a token");
        }
        const size_t hex_length = len(input);
        if (hex_length < 2) {
            throw std::invalid_argument("Hex token not long enough");
        }
        if (hex_length % 2) {
            throw std::invalid_argument("Hex token of odd length");
        }
        const size_t new_length = hex_length / 2 - 1;
        string * result = static_cast<string *>(koreAllocToken(sizeof(string) + new_length));
        set_len(result, new_length);
        for (size_t i = 2; i < hex_length; i += 2) {
            result->data[i / 2 - 1] = (hex_value(input->data[i]) << 4) | hex_value(input->data[i + 1]);
        }
        return result;
    }
}
