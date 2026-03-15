const classifyToken = (value: string): { rank: number; numberValue?: number; textValue: string } => {
  if (/^\d+$/.test(value)) {
    return {
      rank: 0,
      numberValue: Number.parseInt(value, 10),
      textValue: value,
    };
  }

  return {
    rank: 1,
    textValue: value.toLowerCase(),
  };
};

export const compareSegmentedNames = (left: string, right: string): number => {
  const leftParts = left.split('-');
  const rightParts = right.split('-');
  const length = Math.max(leftParts.length, rightParts.length);

  for (let index = 0; index < length; index += 1) {
    const leftPart = leftParts[index];
    const rightPart = rightParts[index];

    if (leftPart === undefined) {
      return -1;
    }
    if (rightPart === undefined) {
      return 1;
    }

    const leftToken = classifyToken(leftPart);
    const rightToken = classifyToken(rightPart);

    if (leftToken.rank !== rightToken.rank) {
      return leftToken.rank - rightToken.rank;
    }

    if (leftToken.rank === 0 && rightToken.rank === 0) {
      if (leftToken.numberValue !== rightToken.numberValue) {
        return (leftToken.numberValue ?? 0) - (rightToken.numberValue ?? 0);
      }
      continue;
    }

    const textComparison = leftToken.textValue.localeCompare(rightToken.textValue);
    if (textComparison !== 0) {
      return textComparison;
    }
  }

  return left.localeCompare(right);
};
