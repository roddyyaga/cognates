from glob import glob

if __name__ == "__main__":
    for filename in glob("python/ARM*csv"):
        print(filename)
        with open(filename) as f:
            lines = f.readlines()
        with open(filename, "w") as f:
            f.write("\n".join(L.strip() for L in lines if not L.strip().startswith("#") and L))
