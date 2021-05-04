"""
BT01
"""
def thapHaNoi(n, toaMot, toaHai, toaBa):

    if n == 1:
        print("Chuyển từ", toaMot, "sang", toaBa)
    else:
        thapHaNoi(n-1,toaMot,toaBa ,toaHai)
        print("chuyển từ", toaMot, "sang", toaBa)
        thapHaNoi(n-1,toaHai,toaMot, toaBa)

thapHaNoi(6, "tòa 1", "tòa 2", "tòa 3")