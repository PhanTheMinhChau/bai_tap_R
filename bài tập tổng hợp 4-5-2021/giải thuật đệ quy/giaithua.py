"""
BT03
"""

def giaithua(n):
    if n < 0:
        return "không tồn tại giai thừa của " + str(n)
    else:
        a = 1
        for i in range(n):
            a = a*(i+1)
        return a
print(giaithua(-4))