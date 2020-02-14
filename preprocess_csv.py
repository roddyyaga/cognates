from glob import glob

if __name__ == "__main__":
    for filename in glob("data/*csv"):
        print(filename)
        with open(filename) as f:
            lines = f.readlines()
        with open(filename, "w") as f:
            f.writelines(L for L in lines if not L.strip().startswith("#"))
