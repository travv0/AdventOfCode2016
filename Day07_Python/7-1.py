import re

def supports_TLS(ip):
    for hypernet_seq in re.finditer("\[(.*?)\]", ip):
        if has_abba("".join(hypernet_seq.groups())):
            return False
    if has_abba(ip):
        return True
    return False

def has_abba(string):
    for i in range(0, len(string) - 3):
        if (string[i] == string[i+3] and
            string[i+1] == string[i+2] and
            string[i] != string[i+1]):
            return True
    return False

def main():
    with open("input.txt") as f:
        ips = f.readlines()
    i = 0
    for ip in ips:
        if supports_TLS(ip):
            i += 1
    return i
