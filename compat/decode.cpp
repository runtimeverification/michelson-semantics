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

std::string getBinaryFromInput(string * input) {
    const std::string binary_with_prefix(input->data, len(input));
    if (binary_with_prefix.size() <= 2 || 
        binary_with_prefix[0] != '0' || 
        binary_with_prefix[1] != 'x') {
        throw std::invalid_argument("Input string invalid (too short or no prefix)");
    }
    return std::string(binary_with_prefix, 2);
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
    const std::string cmd = "python3 " + getExeDirectory() + "/../json-to-source.py " + in_file.name + " > " + out_file.name;
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
            waitForChild(spawnCodecProcess(jsonFile, getBinaryFromInput(input)));
            spawnSourceProcess(jsonFile, sourceFile);
            spawnKastProcess(sourceFile, koreFile);
            return stdStringToKString(readAll(koreFile.fd));
        } catch (const std::invalid_argument e) {
            throw e;
        }
    }
}
