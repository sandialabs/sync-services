import { compareSegmentedNames } from './sortKeys';

describe('compareSegmentedNames', () => {
  it('sorts unsigned numeric segments numerically', () => {
    const names = ['10-alpha', '2-alpha', '1-alpha'];
    const sorted = [...names].sort(compareSegmentedNames);
    expect(sorted).toEqual(['1-alpha', '2-alpha', '10-alpha']);
  });

  it('sorts numeric segments before text segments', () => {
    const names = ['alpha-one', 'alpha-2', 'alpha-10', 'alpha-beta'];
    const sorted = [...names].sort(compareSegmentedNames);
    expect(sorted).toEqual(['alpha-2', 'alpha-10', 'alpha-beta', 'alpha-one']);
  });

  it('compares tuple segments left to right', () => {
    const names = ['2-beta', '10-alpha', '2-alpha'];
    const sorted = [...names].sort(compareSegmentedNames);
    expect(sorted).toEqual(['2-alpha', '2-beta', '10-alpha']);
  });

  it('keeps shorter equal-prefix names first', () => {
    const names = ['section-1-item', 'section-1', 'section-1-part'];
    const sorted = [...names].sort(compareSegmentedNames);
    expect(sorted).toEqual(['section-1', 'section-1-item', 'section-1-part']);
  });
});
