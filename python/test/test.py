import pytest
import os
import time

def path_to_bin():
    stream = os.popen('cabal exec which haskell-lab4-exe')
    bin_p = stream.read()
    bin_p = bin_p.replace('\n', '')
    return bin_p

def test_1():
    os.popen(path_to_bin() +' --use ./test_files/left.use --dox ./test_files/left.dox')
    time.sleep(3)
    file1 = open("./python/test/expected_results/lu_ld_diff.md", "r")
    file2 = open("diff.md", "r")
    expected = file1.read()
    res = file2.read()
    assert(res == expected)


def test_2():
    os.popen(path_to_bin() +' --use ./test_files/right.use --dox ./test_files/left.dox')
    time.sleep(3)
    file1 = open("./python/test/expected_results/ru_ld_diff.md", "r")
    file2 = open("diff.md", "r")
    expected = file1.read()
    res = file2.read()
    assert(res == expected)

def test_3():
    os.popen(path_to_bin() +' --use ./test_files/left.use --dox ./test_files/right.dox')
    time.sleep(3)
    file1 = open("./python/test/expected_results/lu_rd_diff.md", "r")
    file2 = open("diff.md", "r")
    expected = file1.read()
    res = file2.read()
    assert(res == expected)

def test_4():
    os.popen(path_to_bin() +' --use ./test_files/other_right.use --dox ./test_files/right.dox')
    time.sleep(3)
    file1 = open("./python/test/expected_results/ou_rd_diff.md", "r")
    file2 = open("diff.md", "r")
    expected = file1.read()
    res = file2.read()
    assert(res == expected)

def test_5():
    stream = os.popen("rm -f diff.md; rm diff.md")
    expected = stream.read()
    os.popen(path_to_bin() +' --use ./test_files/right.use --dox ./test_files/right.dox')
    time.sleep(3)
    stream = os.popen("rm diff.md")
    res = stream.read()
    assert(res == expected)

def test_6():
    os.popen(path_to_bin() +' --dox ./test_files/right.dox --gu')
    time.sleep(3)
    file1 = open("./python/test/expected_results/generated.use", "r")
    file2 = open("generated.use", "r")
    expected = file1.read()
    res = file2.read()
    assert(res == expected)
