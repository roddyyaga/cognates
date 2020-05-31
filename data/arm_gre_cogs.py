from collections import defaultdict

if __name__ == "__main__":
    with open("ARM_GRE.csv") as f:
        lines = f.readlines()

    glosses = defaultdict(list)
    for line in lines:
        gloss_id = line.split("\t")[3]
        glosses[gloss_id].append(line)

    for cog_set in glosses.values():
        for i, line in enumerate(cog_set[:-1]):
            for other in cog_set[i + 1 :]:
                cog_id, other_cog_id = line.split("\t")[-1], other.split("\t")[-1]
                if cog_id == other_cog_id:
                    print(line.strip("\n"))
                    print(other.strip("\n"))
                    print()
