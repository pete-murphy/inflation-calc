const numberFormat = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 2,
  minimumFractionDigits: 2,
  useGrouping: true,
});

export const format: (value: number) => string = numberFormat.format;
