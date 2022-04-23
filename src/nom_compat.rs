use nom::error::{ErrorKind, ParseError};
use nom::{Err, IResult, Parser};

pub(crate) fn many0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |mut i: I| {
        let mut acc = Vec::with_capacity(4);
        loop {
            match f.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, acc)),
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    if i1 == i {
                        return Err(Err::Error(E::from_error_kind(i, ErrorKind::Many0)));
                    }

                    i = i1;
                    acc.push(o);
                }
            }
        }
    }
}

pub(crate) fn many1<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |mut i: I| match f.parse(i.clone()) {
        Err(Err::Error(err)) => Err(Err::Error(E::append(i, ErrorKind::Many1, err))),
        Err(e) => Err(e),
        Ok((i1, o)) => {
            let mut acc = Vec::with_capacity(4);
            acc.push(o);
            i = i1;

            loop {
                match f.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, acc)),
                    Err(e) => return Err(e),
                    Ok((i1, o)) => {
                        if i1 == i {
                            return Err(Err::Error(E::from_error_kind(i, ErrorKind::Many1)));
                        }

                        i = i1;
                        acc.push(o);
                    }
                }
            }
        }
    }
}

pub(crate) fn many_till<I, O, P, E, F, G>(
    mut f: F,
    mut g: G,
) -> impl FnMut(I) -> IResult<I, (Vec<O>, P), E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    G: Parser<I, P, E>,
    E: ParseError<I>,
{
    move |mut i: I| {
        let mut res = Vec::new();
        loop {
            match g.parse(i.clone()) {
                Ok((i1, o)) => return Ok((i1, (res, o))),
                Err(Err::Error(_)) => {
                    match f.parse(i.clone()) {
                        Err(Err::Error(err)) => {
                            return Err(Err::Error(E::append(i, ErrorKind::ManyTill, err)))
                        }
                        Err(e) => return Err(e),
                        Ok((i1, o)) => {
                            // loop trip must always consume (otherwise infinite loops)
                            if i1 == i {
                                return Err(Err::Error(E::from_error_kind(
                                    i1,
                                    ErrorKind::ManyTill,
                                )));
                            }

                            res.push(o);
                            i = i1;
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }
}
