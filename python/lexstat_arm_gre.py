from lingpy import LexStat  # type: ignore


def main():
    for i, threshold in enumerate([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]):
        print(i)
        lex = LexStat("../data/ARM_GRE.csv")
        lex.get_scorer()
        lex.cluster(
            method="lexstat", threshold=threshold, ref="cognates", verbose=False
        )
        lex.output("csv", filename="ARM_GRE_lexstat_{}".format(i), prettify=False)


if __name__ == "__main__":
    main()
