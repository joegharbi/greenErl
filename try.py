import subprocess

# Define the Erlang command as a string
erlang_command = 'io:format("Hello, World!~n").'

# Start the Erlang shell as a subprocess
proc = subprocess.Popen(['erl'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

# Send the Erlang command to the shell
proc.stdin.write(erlang_command.encode())
proc.stdin.close()

# Read the output of the Erlang shell
output = proc.stdout.read().decode()

# Print the output
print(output)
