const intFormat = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 0,
  minimumFractionDigits: 0,
  useGrouping: true,
});

export const format: (value: number) => string = intFormat.format;
