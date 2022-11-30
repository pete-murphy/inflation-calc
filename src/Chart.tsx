import * as Plot from "@observablehq/plot";
import * as d3 from "d3";
import React from "react";

type Datum = {
  readonly year: number;
  readonly month: string;
  readonly value: number;
};
type Props = {
  readonly data: ReadonlyArray<Datum>;
};

export function _chart(props: Props) {
  const ref = React.useRef();

  React.useEffect(() => {
    const chart = Plot.plot({
      style: {
        background: "transparent",
      },
      y: {
        label: "↑ ($)",
        grid: true,
      },
      color: {
        type: "diverging",
        scheme: "burd",
      },
      marks: [
        Plot.areaY(
          props.data.filter((d) => d.year > 2008),
          {
            x: (d: Datum) => d3.timeParse("%B, %Y")(`${d.month}, ${d.year}`),
            y: (d: Datum) => d.value,
          }
        ),
      ],
    });
    ref.current.append(chart);
    return () => chart.remove();
  }, [props.data]);

  return <div ref={ref}></div>;
}
