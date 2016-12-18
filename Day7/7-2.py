import re

def supports_SSL(ip):
    aba_list = []
    bab_list = []
    for hypernet_seq in re.finditer("\[(.*?)\]", ip):
        aba = list_aba_bab("".join(hypernet_seq.groups()))
        if aba:
            aba_list += aba
    for supernet_seq in re.finditer("(?:^|\])(.*?)(?:\[|$)", ip):
        bab = list_aba_bab("".join(supernet_seq.groups()))
        if bab:
            bab_list += bab
    for aba in aba_list:
        if babify(aba) in bab_list:
            return True
    return False

def list_aba_bab(string):
    l = []
    for i in range(0, len(string) - 2):
        if (string[i] == string[i+2] and string[i] != string[i+1]):
            l.append(string[i:i+3])
    return l

def babify(aba):
    return aba[1] + aba[0] + aba[1]

def main():
    with open("input.txt") as f:
        ips = f.readlines()
    i = 0
    for ip in ips:
        if supports_SSL(ip):
            i += 1
    return i
