import * as fs from "fs/promises";

import D1913 from "./1913.json" assert { type: "json" };
import D1923 from "./1923.json" assert { type: "json" };
import D1933 from "./1933.json" assert { type: "json" };
import D1943 from "./1943.json" assert { type: "json" };
import D1953 from "./1953.json" assert { type: "json" };
import D1963 from "./1963.json" assert { type: "json" };
import D1973 from "./1973.json" assert { type: "json" };
import D1983 from "./1983.json" assert { type: "json" };
import D1993 from "./1993.json" assert { type: "json" };
import D2003 from "./2003.json" assert { type: "json" };
import D2013 from "./2013.json" assert { type: "json" };

const data = [
  D1913,
  D1923,
  D1933,
  D1943,
  D1953,
  D1963,
  D1973,
  D1983,
  D1993,
  D2003,
  D2013,
];

const allJSON = data.flatMap((d) =>
  d.Results.series[0].data
    .map((d_) => ({
      year: +d_.year,
      month: d_.periodName,
      value: +d_.value,
      x: d_.year + d_.period,
    }))
    .sort((a, b) => (a.x < b.x ? -1 : a.x === b.x ? 0 : 1))
    .map(({ year, month, value }) => ({ year, month, value }))
);
const all = "export const allData = " + JSON.stringify(allJSON, null, 2);

const main = () => {
  fs.writeFile("../src/Temp/Data.js", all, "utf-8");
};

main();
