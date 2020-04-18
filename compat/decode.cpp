#include "runtime/header.h"
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <iostream>
#include <libgen.h>

struct TempFile {
    int fd = -1;
    char name[24] = "/tmp/MICHELSON_XXXXXX";

    TempFile() {
        fd = mkstemp(name);
    }

    ~TempFile() {
        unlink(name);
        close(fd);
    }

    TempFile(const TempFile&) = delete;
    TempFile& operator=(const TempFile&) = delete;
};

char getHexValue(const char num) {
    if (0 <= num && num <= 9) {
        return num + '0';
    } else if (10 <= num && num <= 15) {
        return (num - 10) + 'a';
    } else {
        throw std::invalid_argument("Invalid num");
    }
}

std::string getHexFromInput(string * input) {
    const size_t length = len(input);
    std::string ret(2 * length, 0);
    for (size_t i = 0; i < length; ++i) {
        const char byte = input->data[i];
        const char upper = (byte >> 4) & 0x0F;
        const char lower = byte & 0x0F;
        ret[2 * i] = getHexValue(upper);
        ret[(2 * i) + 1] = getHexValue(lower);
    }
    return ret;
}

pid_t spawnCodecProcess(const TempFile& file, const std::string& binary) {
    const pid_t pid = fork();
    if (pid < 0) {
        throw std::invalid_argument("Failed to fork new process");
    } else if (pid == 0) {
        dup2(file.fd, 1);

        char command[] = "tezos-codec";
        char decode[] = "decode";
        char type[] = "005-PsBabyM1.operation.internal";
        char from[] = "from";
        char * arg = strdup(binary.c_str()); 
        // execv, for some reason, takes char * const * rather than const char * const *, so we need to dup

        char * args[] = { command, decode, type, from, arg, nullptr };

        execvp(args[0], args);
        const int error = errno;

        char buffer[1024];
        strerror_r(error, buffer, sizeof(buffer));

        std::cerr << "Unable to exec: " << std::string(buffer) << std::endl;

        free(arg);
        
        exit(EXIT_FAILURE);
    }
    return pid;
}

std::string getExeDirectory() {
    const int MAX_PATH_LENGTH = 4096 + 1;
    char exePath[MAX_PATH_LENGTH];
    memset(exePath, 0, sizeof(exePath));
    readlink("/proc/self/exe", exePath, sizeof(exePath));
    return std::string(dirname(exePath));
}

void spawnSourceProcess(const TempFile& in_file, const TempFile& out_file) {
    const std::string cmd = "python3 " + getExeDirectory() + "/../../json-to-source.py " + in_file.name + " > " + out_file.name;
    if (system(cmd.c_str())) {
        throw std::invalid_argument("Failed to run python source script");
    }
}

void spawnKastProcess(const TempFile& in_file, const TempFile& out_file) {
    const std::string cmd = "kast -s Data --expand-macros -o kore --directory " + getExeDirectory() + "/.. " + std::string(in_file.name) + " > " + std::string(out_file.name);
    if (system(cmd.c_str())) {
        throw std::invalid_argument("Failed to run kast");
    }
}

std::string readAll(const int fd) {
    std::string ret;
    char buffer[8*1024 + 1];
    while (int read_ret = read(fd, buffer, sizeof(buffer) - 1)) {
        if (read_ret < 0) {
            throw std::invalid_argument("Failed to read from pipe");
        } else {
            buffer[read_ret] = '\0';
            ret.append(buffer);
        }
    }
    return ret;
}

void waitForChild(const pid_t pid) {
    int exitStatus = 0;
    pid_t ret = waitpid(pid, &exitStatus, 0);
    if (ret < 0) {
        throw std::invalid_argument("Failed to wait for child process.");
    }

    if (!WIFEXITED(exitStatus) || WEXITSTATUS(exitStatus) != 0) {
        if (WIFEXITED(exitStatus)) {
            std::cerr << "Child exited with status " << WEXITSTATUS(exitStatus) << std::endl;
        } else if (WIFSIGNALED(exitStatus)) {
            std::cerr << "Child signaled " << WTERMSIG(exitStatus) << std::endl;
        }
        throw std::invalid_argument("Child exited with error.");
    }
}

string * stdStringToKString(const std::string& str) {
    string * ret = static_cast<string*>(koreAllocToken(sizeof(string) + str.size()));
    memcpy(ret->data, str.c_str(), str.size());
    set_len(ret, str.size());
    return ret;
}

extern "C" {
    string * hook_MICHELSON_decode(string * input) {
        try {
            const TempFile jsonFile, sourceFile, koreFile;
            waitForChild(spawnCodecProcess(jsonFile, getHexFromInput(input)));
            spawnSourceProcess(jsonFile, sourceFile);
            spawnKastProcess(sourceFile, koreFile);
            return stdStringToKString(readAll(koreFile.fd));
        } catch (const std::invalid_argument e) {
            throw e;
        }
    }

    string * hook_MICHELSON_tohexstring(string * input) {
        const std::string hex = getHexFromInput(input);
        const size_t new_len = hex.size() + 2;
        string * ret = static_cast<string*>(koreAllocToken(sizeof(string) + new_len));
        set_len(ret, new_len);
        ret->data[0] = '0';
        ret->data[1] = 'x';
        for (size_t i = 0; i < new_len; ++i) {
            ret->data[2 + i] = hex[i];
        }
        return ret;
    }
}
