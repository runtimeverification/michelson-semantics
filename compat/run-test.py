#!/usr/bin/python3
import sys, os, json, subprocess, re
from time import sleep

basic_script_name = "babylonnet.sh"
script_directory = os.path.dirname(__file__)

tezos_directory = os.path.join(script_directory, "tezos")
basic_script = os.path.join(script_directory, basic_script_name)
sandbox_node = os.path.join(tezos_directory, "src", "bin_node", "tezos-sandboxed-node.sh")
sandbox_client = os.path.join(tezos_directory, "src", "bin_client", "tezos-init-sandboxed-client.sh")
sandbox_activate = "tezos-activate-alpha" 

class BasicContext(object):
    def __init__(self, script):
        self._script = script
        self._keep_running = self.is_basic_node_running()

    def __enter__(self):
        self.start_basic_node()
        return self

    def __exit__(self, a, b, c):
        if not self._keep_running:
            self.stop_basic_node()

    def _run_command_no_io(self, args):
        with open(os.devnull) as f:  
            subprocess.check_call(args, shell=True, stdin=f, stdout=f, stderr=f)

    def start_basic_node(self):
        self._run_command_no_io(self._script + " start")

    def stop_basic_node(self):
        self._run_command_no_io(self._script + " stop")

    def is_basic_node_running(self):
        proc = subprocess.Popen(self._script + " status", shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (out, err) = proc.communicate()
        out_str = out.decode("utf-8")
        running = "Node is running" in out_str
        not_running = "Node is not running" in out_str
        assert running != not_running, out_str
        return running

    def execute_contract(self, contract):
        assert not contract.needs_sandbox()
        script = contract.get_test_contract()
        typecheck_proc = subprocess.Popen(
                [self._script, "client", "typecheck", "script", script],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
        )
        (t_out, t_err) = typecheck_proc.communicate()
        if typecheck_proc.returncode != 0:
            print("Error during typecheck")
            print(t_out.decode("utf-8"))
            print(t_err.decode("utf-8"))
            print(typecheck_proc.returncode)
            sys.exit(1)
        run_proc = subprocess.Popen(
                [self._script, "client", "run", "script", script, "on", "storage", "Unit", "and", "input", "Unit", "--trace-stack"], 
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
        )
        (out, err) = run_proc.communicate()
        contract.set_result(out.decode("utf-8"))
        return contract
        

class SandboxedContext(object):
    def __init__(self, node, client, activate):
        self._node_script = node
        self._client_script = client
        self._activate_script = activate

    def __enter__(self):
        with open(os.devnull, 'rw') as f:
            self._node = subprocess.Popen([self._node_script, "1", "--connections", "0"], stdin=f, stdout=f, stderr=f)
            sleep(2)
            subprocess.call(["bash", "-c", "'eval `{0}` && {1}; exit $?'".format(self._client_script, self._activate_script)], stdin=f, stdout=f, stderr=f)
        return self

    def __exit__(self, a, b, c):
        self._node.send_signal(2) # SIGINT
        self._node.wait()



final_stack_re = re.compile("\\[\\s*Unit\\s*@exitToken\\s*([^\\]]*)\\]", re.MULTILINE)

class TestContract(object):
    def __init__(self, top_level):
        self._initial_stack = top_level["initial_stack"]
        self._code = top_level["code"]
        self._final_stack = top_level["final_stack"]
        self._test_contract = self._create_test_contract()
        self._success = None
        self._final_stack = None

    def _initial_stack_to_string(self):
        pushes = ["PUSH {0} {1}".format(o["type"], o["value"]) for o in self._initial_stack]
        pushes.reverse()
        return "; ".join(pushes)

    def _code_to_string(self):
        return self._code.replace("\n", " ")

    def _create_test_contract(self):
        TEMPLATE_CONTRACT = """parameter unit ;
storage unit ;
code {{ DROP ;
       {{ {0} ; }} ;
       {{ {1} }} ;
       UNIT @exitToken ;
       FAILWITH ; }}
"""
        return TEMPLATE_CONTRACT.format(self._initial_stack_to_string(), self._code_to_string())

    def get_test_contract(self):
        return self._test_contract

    def needs_sandbox(self):
        return False

    def did_succeed(self):
        return self._success

    def get_final_stack(self):
        return self._final_stack

    def set_result(self, output_string):
        self._success = "@exitToken" in output_string
        if self._success:
            final_stack_string = final_stack_re.search(output_string).group(1)
            self._final_stack = list(map(lambda x: x.strip(), final_stack_string.split("\n"))) # First element is top of the stack

def load_test_contract(file_name):
    with open(file_name, 'r') as f:
        return TestContract(json.load(f))

if __name__ == "__main__":
    with BasicContext(basic_script) as bc:
        for test_file in sys.argv[1:]:
            print(bc.execute_contract(load_test_contract(test_file)).get_final_stack())
