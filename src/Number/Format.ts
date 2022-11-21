const numberFormat = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 2,
  minimumFractionDigits: 2,
  useGrouping: true,
});

export const formatNumber: (value: number) => string = numberFormat.format;

const intFormat = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 0,
  minimumFractionDigits: 0,
  useGrouping: true,
});
export const formatInt: (value: number) => string = intFormat.format;
