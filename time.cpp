#include <locale>
#include <sstream>
#include <ctime>
#include <iomanip>
#include "runtime/header.h"

extern "C" {
    const mpz_ptr hook_TIME_ISO2Epoch(string *iso) {
        std::tm time; 
        std::istringstream ss(iso->data);
        ss >> std::get_time(&time, "%Y-%m-%dT%H:%M:%SZ");
        if (ss.fail()) {
            throw std::invalid_argument("Input string did not match ISO-8601");
        }
        time.tm_isdst = 0;
        mpz_t result;
        mpz_init_set_ui(result, static_cast<unsigned long>(std::mktime(&time) - timezone));
        return move_int(result);
    }
}
