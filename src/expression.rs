use crate::{
    ByteString, CounterContext, CounterContextMut, Counters, LocalCounter, NumericCounter,
    StringCounter,
};

pub trait CounterContextExt {
    fn local_counter(&self, counter: LocalCounter) -> i32;
    fn numeric_counter(&self, counter: NumericCounter) -> i32;
    fn string_counter(&self, counter: StringCounter) -> ByteString;
}

impl<'a> CounterContextExt for CounterContext<'a> {
    fn local_counter(&self, counter: LocalCounter) -> i32 {
        CounterContext::local_counter(self, counter)
    }
    fn numeric_counter(&self, counter: NumericCounter) -> i32 {
        CounterContext::numeric_counter(self, counter)
    }
    fn string_counter(&self, counter: StringCounter) -> ByteString {
        CounterContext::string_counter(self, counter)
    }
}

pub trait CounterContextMutExt: CounterContextExt {
    fn as_immutable(&self) -> &dyn CounterContextExt;
    fn local_counter_mut(&mut self, counter: LocalCounter) -> Option<&mut i32>;
    fn numeric_counter_mut(&mut self, counter: NumericCounter) -> Option<&mut i32>;
}

impl<'a> CounterContextExt for CounterContextMut<'a> {
    fn local_counter(&self, counter: LocalCounter) -> i32 {
        self.as_immutable().local_counter(counter)
    }
    fn numeric_counter(&self, counter: NumericCounter) -> i32 {
        self.as_immutable().numeric_counter(counter)
    }
    fn string_counter(&self, counter: StringCounter) -> ByteString {
        self.as_immutable().string_counter(counter)
    }
}

impl<'a> CounterContextMutExt for CounterContextMut<'a> {
    fn as_immutable(&self) -> &dyn CounterContextExt {
        self as &dyn CounterContextExt
    }

    fn local_counter_mut(&mut self, counter: LocalCounter) -> Option<&mut i32> {
        CounterContextMut::local_counter_mut(self, counter)
    }

    fn numeric_counter_mut(&mut self, counter: NumericCounter) -> Option<&mut i32> {
        CounterContextMut::numeric_counter_mut(self, counter)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ExprOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Pow,
    BitOr,
    BitAnd,
    BitXor,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    BitShiftLeft,
    BitShiftRight,
    //TODO: arithmetic shift right (>>>)
}

enum Token {
    Constant(i32),
    Operator(ExprOp),
    Variable(Vec<u8>),
}

#[derive(Debug, PartialEq)]
enum ExprState {
    Start,
    Constant(i32),
    Operator(ExprOp),
    Variable(Vec<u8>),
    VariableName(Vec<u8>),
    End,
}

impl ExprState {
    fn transition(self, byte: Option<u8>) -> Result<(ExprState, Option<Token>), ()> {
        match self {
            ExprState::Start => {
                let byte = match byte {
                    Some(b) => b,
                    None => return Ok((ExprState::End, None)),
                };
                if byte == b' ' {
                    Ok((ExprState::Start, None))
                } else if byte >= b'0' && byte <= b'9' {
                    Ok((ExprState::Constant((byte - b'0') as i32), None))
                } else if byte == b'\'' {
                    Ok((ExprState::Variable(vec![]), None))
                } else if byte == b'-' {
                    Ok((ExprState::Operator(ExprOp::Minus), None))
                } else {
                    Err(())
                }
            }

            ExprState::Constant(v) => {
                let byte = match byte {
                    Some(b) => b,
                    None => {
                        return Ok((ExprState::End, Some(Token::Constant(v))));
                    }
                };
                if byte == b' ' {
                    Ok((ExprState::Constant(v), None))
                } else if byte >= b'0' && byte <= b'9' {
                    Ok((ExprState::Constant(v * 10 + (byte - b'0') as i32), None))
                } else if byte == b'+' {
                    Ok((ExprState::Operator(ExprOp::Plus), Some(Token::Constant(v))))
                } else if byte == b'-' {
                    Ok((ExprState::Operator(ExprOp::Minus), Some(Token::Constant(v))))
                } else if byte == b'*' {
                    Ok((
                        ExprState::Operator(ExprOp::Multiply),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'/' {
                    Ok((
                        ExprState::Operator(ExprOp::Divide),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'%' {
                    Ok((
                        ExprState::Operator(ExprOp::Modulo),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'^' {
                    Ok((ExprState::Operator(ExprOp::Pow), Some(Token::Constant(v))))
                } else if byte == b'a' {
                    Ok((
                        ExprState::Operator(ExprOp::BitAnd),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'o' {
                    Ok((ExprState::Operator(ExprOp::BitOr), Some(Token::Constant(v))))
                } else if byte == b'x' {
                    Ok((
                        ExprState::Operator(ExprOp::BitXor),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'>' {
                    Ok((
                        ExprState::Operator(ExprOp::Greater),
                        Some(Token::Constant(v)),
                    ))
                } else if byte == b'<' {
                    Ok((ExprState::Operator(ExprOp::Less), Some(Token::Constant(v))))
                } else if byte == b'=' {
                    Ok((ExprState::Operator(ExprOp::Equal), Some(Token::Constant(v))))
                } else if byte == b'!' {
                    Ok((
                        ExprState::Operator(ExprOp::NotEqual),
                        Some(Token::Constant(v)),
                    ))
                } else {
                    Err(())
                }
            }

            ExprState::Operator(op) => {
                let byte = match byte {
                    Some(b) => b,
                    None => return Ok((ExprState::End, None)),
                };
                // TODO: !, unary
                if byte == b' ' {
                    Ok((ExprState::Operator(op), None))
                } else if byte >= b'0' && byte <= b'9' {
                    Ok((
                        ExprState::Constant((byte - b'0') as i32),
                        Some(Token::Operator(op)),
                    ))
                } else if byte == b'\'' {
                    Ok((ExprState::Variable(vec![]), Some(Token::Operator(op))))
                } else if byte == b'=' {
                    match op {
                        ExprOp::Greater => Ok((ExprState::Operator(ExprOp::GreaterEqual), None)),
                        ExprOp::Less => Ok((ExprState::Operator(ExprOp::LessEqual), None)),
                        // FIXME: this allows !============, also ! without following =
                        ExprOp::NotEqual => Ok((ExprState::Operator(ExprOp::NotEqual), None)),
                        _ => Err(()),
                    }
                } else if byte == b'<' {
                    if op == ExprOp::Less {
                        Ok((ExprState::Operator(ExprOp::BitShiftLeft), None))
                    } else {
                        Err(())
                    }
                } else if byte == b'>' {
                    if op == ExprOp::Greater {
                        Ok((ExprState::Operator(ExprOp::BitShiftRight), None))
                    } else {
                        Err(())
                    }
                } else {
                    Err(())
                }
            }

            ExprState::Variable(mut var) => {
                let byte = match byte {
                    Some(b) => b,
                    None => return Err(()),
                };
                if byte != b'\'' {
                    var.push(byte);
                    Ok((ExprState::Variable(var), None))
                } else {
                    Ok((ExprState::VariableName(var), None))
                }
            }

            ExprState::VariableName(var) => {
                let byte = match byte {
                    Some(b) => b,
                    None => {
                        return Ok((ExprState::End, Some(Token::Variable(var))));
                    }
                };
                if byte == b' ' {
                    Ok((ExprState::VariableName(var), None))
                } else if byte == b'+' {
                    Ok((
                        ExprState::Operator(ExprOp::Plus),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'-' {
                    Ok((
                        ExprState::Operator(ExprOp::Minus),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'*' {
                    Ok((
                        ExprState::Operator(ExprOp::Multiply),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'/' {
                    Ok((
                        ExprState::Operator(ExprOp::Divide),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'%' {
                    Ok((
                        ExprState::Operator(ExprOp::Modulo),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'^' {
                    Ok((ExprState::Operator(ExprOp::Pow), Some(Token::Variable(var))))
                } else if byte == b'a' {
                    Ok((
                        ExprState::Operator(ExprOp::BitAnd),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'o' {
                    Ok((
                        ExprState::Operator(ExprOp::BitOr),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'x' {
                    Ok((
                        ExprState::Operator(ExprOp::BitXor),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'>' {
                    Ok((
                        ExprState::Operator(ExprOp::Greater),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'<' {
                    Ok((
                        ExprState::Operator(ExprOp::Less),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'=' {
                    Ok((
                        ExprState::Operator(ExprOp::Equal),
                        Some(Token::Variable(var)),
                    ))
                } else if byte == b'!' {
                    Ok((
                        ExprState::Operator(ExprOp::NotEqual),
                        Some(Token::Variable(var)),
                    ))
                } else {
                    Err(())
                }
            }

            ExprState::End => Err(()),
        }
    }
}

pub(crate) fn evaluate_expression(
    expr: &[u8],
    counters: &Counters,
    context: &dyn CounterContextExt,
) -> Result<ByteString, ByteString> {
    let mut tokens = vec![];
    let mut state = ExprState::Start;
    let mut idx = 0usize;
    let mut input = expr.to_vec();
    loop {
        let c = match input.get(idx) {
            Some(c) => *c,
            None => break,
        };

        // Handle nested expressions:
        if c == b'(' {
            let mut end = None;
            let mut nesting = 0;
            let start = idx;
            // Find the complete expression string, including all sub-nested expressions:
            for (j, c) in input[start..].iter().enumerate() {
                if *c == b'(' {
                    nesting += 1;
                } else if *c == b')' {
                    nesting -= 1;
                    if nesting == 0 {
                        end = Some(start + j);
                        break;
                    }
                }
            }

            // If there is a complete expression present, evaluate it and replace it
            // with the evaluated value, then continue parsing the result.
            if let Some(end) = end {
                let result = evaluate_expression(&input[start + 1..end], counters, context)?;
                input.splice(start..=end, result.0.into_iter());
                continue;
            }
        }

        // Feed the current byte of the expression to the parser.
        let (new_state, token) = match state.transition(Some(c)) {
            Ok(a) => a,
            Err(()) => {
                warn!(
                    "Error evaluating {} at {}",
                    std::str::from_utf8(expr).unwrap(),
                    c as char
                );
                return Err(input.into());
            }
        };
        state = new_state;
        if let Some(token) = token {
            tokens.push(token);
        }

        // Parse the next byte of the expression.
        idx += 1;
    }

    // Notify the parser that we're at the end of the expression input.
    let (state, token) = match state.transition(None) {
        Ok(a) => a,
        Err(()) => {
            warn!("Error ending {}", std::str::from_utf8(expr).unwrap());
            return Err(input.into());
        }
    };
    assert_eq!(state, ExprState::End);
    if let Some(token) = token {
        tokens.push(token);
    }

    // Process the tokens that were parsed from the expression input.
    let mut value = None;
    let mut current_op = None;
    // TODO: handle dividing by a negative number
    for token in tokens {
        match token {
            Token::Constant(v) => match (value, current_op) {
                (None, None) | (None, Some(ExprOp::Plus)) => {
                    value = Some(v);
                    current_op = None;
                }
                (None, Some(ExprOp::Minus)) => {
                    value = Some(-v);
                    current_op = None;
                }
                (Some(current_val), Some(o)) => {
                    match o {
                        ExprOp::Plus => value = Some(current_val + v),
                        ExprOp::Minus => value = Some(current_val - v),
                        ExprOp::Multiply => value = Some(current_val * v),
                        ExprOp::Divide => {
                            value = if v != 0 {
                                Some(current_val / v)
                            } else {
                                Some(0)
                            }
                        }
                        ExprOp::Modulo => {
                            value = if v != 0 {
                                Some(current_val % v)
                            } else {
                                Some(0)
                            }
                        }
                        ExprOp::Pow => value = Some(current_val.pow(v as u32)), // FIXME: what does ^ with negative values?
                        ExprOp::BitAnd => value = Some(current_val & v),
                        ExprOp::BitOr => value = Some(current_val | v),
                        ExprOp::BitXor => value = Some(current_val ^ v),
                        ExprOp::BitShiftLeft => value = Some(current_val << v),
                        ExprOp::BitShiftRight => value = Some(current_val >> v),
                        ExprOp::Greater => value = Some((current_val > v) as i32),
                        ExprOp::GreaterEqual => value = Some((current_val >= v) as i32),
                        ExprOp::Less => value = Some((current_val < v) as i32),
                        ExprOp::LessEqual => value = Some((current_val <= v) as i32),
                        ExprOp::Equal => value = Some((current_val == v) as i32),
                        ExprOp::NotEqual => value = Some((current_val != v) as i32),
                    }
                    current_op = None;
                }
                (Some(_), None) | (None, Some(_)) => unimplemented!(),
            },

            Token::Variable(name) => {
                let var_val = counters.get(&ByteString(name), context);
                match (value, current_op) {
                    (None, None) | (None, Some(ExprOp::Plus)) => {
                        value = Some(var_val);
                        current_op = None;
                    }
                    (None, Some(ExprOp::Minus)) => {
                        value = Some(-var_val);
                        current_op = None;
                    }
                    (Some(current_val), Some(o)) => {
                        match o {
                            ExprOp::Plus => value = Some(current_val + var_val),
                            ExprOp::Minus => value = Some(current_val - var_val),
                            ExprOp::Multiply => value = Some(current_val * var_val),
                            ExprOp::Divide => {
                                value = if var_val != 0 {
                                    Some(current_val / var_val)
                                } else {
                                    Some(0)
                                }
                            }
                            ExprOp::Modulo => {
                                value = if var_val != 0 {
                                    Some(current_val % var_val)
                                } else {
                                    Some(0)
                                }
                            }
                            ExprOp::Pow => value = Some(current_val.pow(var_val as u32)), //FIXME:  what does ^ do with negative values?
                            ExprOp::BitAnd => value = Some(current_val & var_val),
                            ExprOp::BitOr => value = Some(current_val | var_val),
                            ExprOp::BitXor => value = Some(current_val ^ var_val),
                            ExprOp::BitShiftLeft => value = Some(current_val << var_val),
                            ExprOp::BitShiftRight => value = Some(current_val >> var_val),
                            ExprOp::Greater => value = Some((current_val > var_val) as i32),
                            ExprOp::GreaterEqual => value = Some((current_val >= var_val) as i32),
                            ExprOp::Less => value = Some((current_val < var_val) as i32),
                            ExprOp::LessEqual => value = Some((current_val <= var_val) as i32),
                            ExprOp::Equal => value = Some((current_val == var_val) as i32),
                            ExprOp::NotEqual => value = Some((current_val != var_val) as i32),
                        }
                        current_op = None;
                    }
                    (Some(_), None) | (None, Some(_)) => unimplemented!(),
                }
            }

            Token::Operator(o) => match (value, current_op) {
                (Some(_), None) | (None, None) => current_op = Some(o),
                (None, Some(_)) | (Some(_), Some(_)) => unimplemented!(),
            },
        }
    }

    Ok(ByteString(value.unwrap_or(0).to_string().into_bytes()))
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;

    pub(crate) struct TestLocalCounters;

    impl CounterContextExt for TestLocalCounters {
        fn local_counter(&self, _counter: LocalCounter) -> i32 {
            unimplemented!()
        }
        fn string_counter(&self, _counter: StringCounter) -> ByteString {
            unimplemented!()
        }
        fn numeric_counter(&self, _counter: NumericCounter) -> i32 {
            unimplemented!()
        }
    }

    impl CounterContextMutExt for TestLocalCounters {
        fn as_immutable(&self) -> &dyn CounterContextExt {
            self as &dyn CounterContextExt
        }
        fn local_counter_mut(&mut self, _counter: LocalCounter) -> Option<&mut i32> {
            unimplemented!()
        }
    }

    #[test]
    fn expr_constant() {
        assert_eq!(
            evaluate_expression(b"5", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"5"
        );
    }

    #[test]
    fn expr_add_constants() {
        assert_eq!(
            evaluate_expression(b"5 + 10", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"15"
        );
    }

    #[test]
    fn expr_add_multiple_constants() {
        assert_eq!(
            evaluate_expression(b"5 + 10 + 20", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"35"
        );
    }

    #[test]
    fn expr_sub_multiple_constants() {
        assert_eq!(
            evaluate_expression(b"5 - 10 - 20", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"-25"
        );
    }

    #[test]
    fn expr_leading_neg() {
        assert_eq!(
            evaluate_expression(b"-5", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"-5"
        );
    }

    #[test]
    fn expr_mult() {
        assert_eq!(
            evaluate_expression(b"-5 * 5", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"-25"
        );
    }

    #[test]
    fn expr_div() {
        assert_eq!(
            evaluate_expression(b"-5 / 5", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"-1"
        );
    }

    #[test]
    fn expr_mod() {
        assert_eq!(
            evaluate_expression(b"12 % 5", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"2"
        );
    }

    #[test]
    fn expr_var() {
        let mut counters = Counters::new();
        counters.set("countername".into(), &mut TestLocalCounters, 5);
        assert_eq!(
            evaluate_expression(b"'countername'", &counters, &TestLocalCounters)
                .unwrap()
                .0,
            b"5"
        );
    }

    #[test]
    fn expr_var_add() {
        let mut counters = Counters::new();
        counters.set("countername".into(), &mut TestLocalCounters, 50);
        assert_eq!(
            evaluate_expression(b"'countername' - 10", &counters, &TestLocalCounters)
                .unwrap()
                .0,
            b"40"
        );
    }

    #[test]
    fn expr_inner_expression() {
        assert_eq!(
            evaluate_expression(b"(5 * 3)", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"15"
        );
    }

    #[test]
    fn expr_subexpression() {
        assert_eq!(
            evaluate_expression(b"10 - (5 * 3)", &Counters::new(), &TestLocalCounters)
                .unwrap()
                .0,
            b"-5"
        );
    }

    #[test]
    fn expr_var_subexpression() {
        let mut counters = Counters::new();
        counters.set("boo".into(), &mut TestLocalCounters, 5);
        counters.set("countername5".into(), &mut TestLocalCounters, 50);
        assert_eq!(
            evaluate_expression(b"'countername('boo')'", &counters, &TestLocalCounters)
                .unwrap()
                .0,
            b"50"
        );
    }
}
