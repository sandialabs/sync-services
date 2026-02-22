import { prettifyScheme } from './scheme';

describe('prettifyScheme', () => {
  describe('basic expressions', () => {
    it('should format a simple expression', () => {
      const input = '(+ 1 2)';
      const result = prettifyScheme(input);
      expect(result).toBe('(+ 1 2)');
    });

    it('should format nested expressions', () => {
      const input = '(+ 1 (* 2 3))';
      const result = prettifyScheme(input);
      expect(result).toBe('(+ 1\n  (* 2 3))');
    });

    it('should handle empty input', () => {
      const result = prettifyScheme('');
      expect(result).toBe('');
    });

    it('should handle whitespace-only input', () => {
      const result = prettifyScheme('   \n\t  ');
      expect(result).toBe('');
    });
  });

  describe('string literals', () => {
    it('should preserve string literals', () => {
      const input = '(display "hello world")';
      const result = prettifyScheme(input);
      expect(result).toBe('(display "hello world")');
    });

    it('should handle escaped characters in strings', () => {
      const input = '(display "hello\\"world")';
      const result = prettifyScheme(input);
      expect(result).toBe('(display "hello\\"world")');
    });

    it('should handle strings with newlines', () => {
      const input = '(display "line1\\nline2")';
      const result = prettifyScheme(input);
      expect(result).toBe('(display "line1\\nline2")');
    });
  });

  describe('comments', () => {
    it('should preserve comments on their own line', () => {
      const input = '; this is a comment\n(+ 1 2)';
      const result = prettifyScheme(input);
      expect(result).toContain('; this is a comment');
      expect(result).toContain('(+ 1 2)');
    });

    it('should handle inline comments', () => {
      const input = '(+ 1 2) ; add numbers';
      const result = prettifyScheme(input);
      expect(result).toContain('(+ 1 2)');
      expect(result).toContain('; add numbers');
    });
  });

  describe('quote syntax', () => {
    it('should handle single quote', () => {
      const input = "'(1 2 3)";
      const result = prettifyScheme(input);
      expect(result).toBe("'(1 2 3)");
    });

    it('should handle quasiquote', () => {
      const input = '`(1 2 3)';
      const result = prettifyScheme(input);
      expect(result).toBe('`(1 2 3)');
    });

    it('should handle unquote', () => {
      const input = '`(1 ,x 3)';
      const result = prettifyScheme(input);
      // The prettifier may or may not preserve space before ,x
      expect(result).toContain('`(1');
      expect(result).toContain(',x');
      expect(result).toContain('3)');
    });

    it('should handle unquote-splicing', () => {
      const input = '`(1 ,@xs 3)';
      const result = prettifyScheme(input);
      // The prettifier may or may not preserve space before ,@xs
      expect(result).toContain('`(1');
      expect(result).toContain(',@xs');
      expect(result).toContain('3)');
    });
  });

  describe('byte vectors (#u syntax)', () => {
    it('should keep simple byte vector together', () => {
      const input = '#u(1 2 3)';
      const result = prettifyScheme(input);
      expect(result).toBe('#u(1 2 3)');
    });

    it('should keep byte vector with hex values together', () => {
      const input = '#u(0 255 128)';
      const result = prettifyScheme(input);
      expect(result).toBe('#u(0 255 128)');
    });

    it('should handle byte vector in expression', () => {
      const input = '(sync-digest #u(1 2 3))';
      const result = prettifyScheme(input);
      expect(result).toBe('(sync-digest #u(1 2 3))');
    });

    it('should handle multiple byte vectors', () => {
      const input = '(sync-cons #u(0) #u(1))';
      const result = prettifyScheme(input);
      expect(result).toBe('(sync-cons #u(0) #u(1))');
    });

    it('should handle nested expressions with byte vectors', () => {
      const input = '(sync-cons (sync-cons #u(0) #u(1)) (sync-cons #u(2) #u(3)))';
      const result = prettifyScheme(input);
      expect(result).toContain('#u(0)');
      expect(result).toContain('#u(1)');
      expect(result).toContain('#u(2)');
      expect(result).toContain('#u(3)');
    });

    it('should handle empty byte vector', () => {
      const input = '#u()';
      const result = prettifyScheme(input);
      expect(result).toBe('#u()');
    });
  });

  describe('complex expressions', () => {
    it('should format let expressions', () => {
      const input = '(let ((x 1) (y 2)) (+ x y))';
      const result = prettifyScheme(input);
      expect(result).toContain('let');
      expect(result).toContain('x 1');
      expect(result).toContain('y 2');
    });

    it('should format define expressions', () => {
      const input = '(define (square x) (* x x))';
      const result = prettifyScheme(input);
      expect(result).toContain('define');
      expect(result).toContain('square');
    });

    it('should format lambda expressions', () => {
      const input = '(lambda (x) (* x x))';
      const result = prettifyScheme(input);
      expect(result).toContain('lambda');
    });

    it('should handle deeply nested expressions', () => {
      const input = '(a (b (c (d (e)))))';
      const result = prettifyScheme(input);
      expect(result).toContain('(a');
      expect(result).toContain('(b');
      expect(result).toContain('(c');
      expect(result).toContain('(d');
      expect(result).toContain('(e)');
    });
  });

  describe('whitespace handling', () => {
    it('should normalize excessive whitespace', () => {
      const input = '(+    1     2)';
      const result = prettifyScheme(input);
      expect(result).toBe('(+ 1 2)');
    });

    it('should handle tabs and newlines', () => {
      const input = '(+\t1\n\t2)';
      const result = prettifyScheme(input);
      expect(result).toBe('(+ 1 2)');
    });
  });
});
