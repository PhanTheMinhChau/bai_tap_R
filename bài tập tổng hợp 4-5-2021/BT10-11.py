from binarytree import build
values = [7, 3, 2, 6, 9, None, 1, 5, 8]
root = build(values)
print(root)
print(root.preorder) # duyệt cây theo thứ tự trước
print(root.postorder) # duyệt cây theo thứ tự sau