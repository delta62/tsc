#[macro_use]
extern crate lazy_static;
extern crate regex;

use regex::Regex;

pub enum UnicodeProperty {
    IdContinue,
    IdStart,
}

pub fn is(c: char, prop: UnicodeProperty) -> bool {
    match prop {
        UnicodeProperty::IdContinue => is_id_continue(c),
        UnicodeProperty::IdStart    => is_id_start(c),
    }
}

fn is_id_start(c: char) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\p{ID_Start}").unwrap();
    }
    RE.is_match(&c.to_string())
}

fn is_id_continue(c: char) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\p{ID_Continue}").unwrap();
    }
    RE.is_match(&c.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn omits_whitespace_from_id_start() {
        assert!(!is_id_start(' '));
    }

    #[test]
    fn omits_whitespace_from_id_continue() {
        assert!(!is_id_continue(' '));
    }
}
