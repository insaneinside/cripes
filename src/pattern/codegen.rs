//! Various helpers for code-generation.

use std::io;
#[cfg(feature = "pattern_class")] use std::cmp;
use std::char;
use std::iter;
use std::usize;
#[cfg(all(test, feature = "pattern_class"))]
use std::iter::FromIterator;

#[cfg(feature = "pattern_class")]
use super::{Atom, Class, ClassMember};

use super::SizeBound;

// ================================================================
/// Trait for atom types that can be converted to byte sequences.
pub trait Bytes<'a> {
    /// Iterator type returned by `bytes`.
    type Iter: 'a + Iterator<Item=u8>;

    /// Fetch an iterator over "self" as a series of bytes.
    fn bytes(&self) -> Self::Iter;
}

/// Interface for types which can bound the number of bytes required to match
/// a particular instance of themselves.
pub trait SizedRead {
    /// Get the size of this object when read directly from something
    /// implementing `std::io::Read`.
    fn read_size(&self) -> SizeBound;
}

/// Trait for use with fixed-size types
pub trait ReadToBuffer {
    /// Read at most `max_count` instances of this atom type from the given
    /// stream, placing them into the supplied buffer.
    fn read_to_buffer<R>(r: &mut R, dest: &mut [u8], max_count: usize) -> io::Result<usize>
        where R: io::Read;
}


// ----------------------------------------------------------------
// Implementations

impl<'a> Bytes<'a> for u8 {
    type Iter = iter::Once<u8>;
    fn bytes(&self) -> Self::Iter {
        iter::once(*self)
    }
}


impl SizedRead for u8 {
    #[inline(always)]
    fn read_size(&self) -> SizeBound {
        SizeBound::Exact(1)
    }
}

impl ReadToBuffer for u8 {
    #[inline(always)]
    fn read_to_buffer<R>(r: &mut R, dest: &mut [u8], max_count: usize) -> io::Result<usize>
        where R: io::Read {
        r.read(&mut dest[0..max_count])
    }
}

// ----------------------------------------------------------------

// This table was copied from the Rust source tree,
// file "src/libcore/str/mod.rs".
// https://tools.ietf.org/html/rfc3629
static UTF8_CHAR_WIDTH: [u8; 256] = [
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x1F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x3F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x5F
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x7F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x9F
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xBF
0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // 0xDF
3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // 0xEF
4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, // 0xFF
];

/*impl<'a> Bytes<'a> for char {
    type Iter = ::std::char::EncodeUtf8;
    fn bytes(&self) -> Self::Iter {
        self.encode_utf8()
    }
}*/

impl SizedRead for char {
    #[inline]
    fn read_size(&self) -> SizeBound {
        self.len_utf8().into()
    }
}

impl ReadToBuffer for char {
    #[inline]
    fn read_to_buffer<R>(r: &mut R, dest: &mut [u8], max_count: usize) -> io::Result<usize>
        where R: io::Read {
        let mut i = 0;
        let mut n = 0;
        while i < dest.len() && n < max_count {
            try!(r.read_exact(&mut dest[i..(i+1)]));
            let w = UTF8_CHAR_WIDTH[dest[i] as usize];
            try!(r.read_exact(&mut dest[(i+1)..(i + w as usize)]));
            i += w as usize;
            n += 1;
        }
        Ok(i)
    }

}


#[cfg(test)]
#[test]
fn test_sizedread_char() {
    assert_eq!(SizeBound::Exact(1), 'a'.read_size());
    assert_eq!(SizeBound::Exact(3), '✓'.read_size());
}


#[cfg(test)]
#[test]
fn test_readable_char() {
    let mut buf = ['\0' as u8; 32];
    {
        let mut r = io::repeat('a' as u8);
        assert_eq!(3, char::read_to_buffer(&mut r, &mut buf, 3).unwrap());
        assert_eq!(&['a' as u8 as u8, 'a' as u8, 'a' as u8, '\0' as u8], &buf[0..4]);
    }

    {
        let s = "✓☠⚔";
        let mut r = io::Cursor::new(s.as_bytes());
        assert_eq!(9, char::read_to_buffer(&mut r, &mut buf, 3).unwrap());
        assert_eq!(s.as_bytes(), &buf[0..9]);
    }
}

// ----------------------------------------------------------------

/*impl<T> SizedRead for Transition<T>
    where T: Atom + SizedRead
{
    fn read_size(&self) -> SizeBound {
        match self {
            &Transition::Atom(a) => a.read_size(),
            &Transition::Literal(ref atoms) => atoms.iter().map(|a| a.read_size()).sum(),
            &Transition::Wildcard => SizeBound::Range(1, 4),
            &Transition::Anchor(..) => SizeBound::Exact(0),
            &Transition::Class(ref c) => c.read_size(),
        }
    }
}*/

#[cfg(test)]
#[test]
fn test_sizedread() {
    assert_eq!(SizeBound::Exact(1), 'a'.read_size());
    assert_eq!(SizeBound::Exact(3), '✓'.read_size());
    #[cfg(feature = "pattern_class")]
    assert_eq!(SizeBound::Range(1, 3), Class::from_iter(['x', 'y', '⚔'].into_iter().cloned()).read_size());
}

// ----------------------------------------------------------------

#[cfg(feature = "pattern_class")]
impl<T> SizedRead for ClassMember<T>
    where T: Atom + SizedRead
{
    fn read_size(&self) -> SizeBound {
        match self {
            &ClassMember::Atom(a) => a.read_size(),
            &ClassMember::Range(first, last) => {
                // We'll assume that any differences in read size between
                // `first` and `last` follow a linear function between the two;
                // this _assumption_ allows us to avoid evaluating `read_size`
                // for every single member of the range.
                let frs = first.read_size();
                let lrs = last.read_size();
                (cmp::min(frs.min(), lrs.min())..(cmp::max(frs.max(), lrs.max()) + 1)).into()
            }
        }
    }
}

#[cfg(feature = "pattern_class")]
impl<T> SizedRead for Class<T>
    where T: Atom + SizedRead
{
    fn read_size(&self) -> SizeBound {
        let mut min = usize::MAX;
        let mut max = usize::MIN;

        for m in self.iter_members() {
            let rs = m.read_size();
            min = cmp::min(min, rs.min());
            max = cmp::max(max, rs.max());
        }

        (min..(max + 1)).into()
    }
}
