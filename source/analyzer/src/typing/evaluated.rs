use crate::{
    unify_types, utils::coerce, IntermediateType, ParameterType, PathIndex, ProgramError,
    SemanticSymbolKind, SymbolIndex, SymbolTable, UnifyOptions,
};
use ast::Span;
use errors::{
    value_as_type,
    TypeError,
    // TypeErrorType
};

/// A type expression, as is.
#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedType {
    /// A type that evolves to different values, based on control flow.
    Partial {
        types: Vec<EvaluatedType>,
    },
    /// An instance created with `new A()`, or by labelling a value with `: A`.
    ModelInstance {
        model: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    TraitInstance {
        trait_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// An instance of an enum created by assigning a variant.
    EnumInstance {
        enum_: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A named or anonymous function.
    FunctionInstance {
        function: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    FunctionExpressionInstance {
        is_async: bool,
        params: Vec<ParameterType>,
        return_type: Box<EvaluatedType>,
        generic_args: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A method.
    MethodInstance {
        method: SymbolIndex,
        generic_arguments: Vec<(SymbolIndex, EvaluatedType)>,
    },
    /// A model value.
    Model(SymbolIndex),
    Trait(SymbolIndex),
    Enum(SymbolIndex),
    Module(SymbolIndex),
    OpaqueTypeInstance {
        methods: Vec<SymbolIndex>,
        properties: Vec<SymbolIndex>,
        implementations: Vec<SymbolIndex>,
        collaborators: Vec<SymbolIndex>,
    },
    Void,
    Never,
    Unknown,
    Generic {
        base: SymbolIndex,
    },
    Borrowed {
        base: Box<EvaluatedType>,
    },
}

impl EvaluatedType {
    /// Returns `true` if the evaluated type is [`Model`].
    ///
    /// [`Model`]: EvaluatedType::Model
    #[must_use]
    pub fn is_model(&self) -> bool {
        matches!(self, Self::Model(..))
    }

    /// Returns `true` if the evaluated type is [`Trait`].
    ///
    /// [`Trait`]: EvaluatedType::Trait
    #[must_use]
    pub fn is_trait(&self) -> bool {
        matches!(self, Self::Trait(..))
    }

    /// Returns `true` if the evaluated type is [`Enum`].
    ///
    /// [`Enum`]: EvaluatedType::Enum
    #[must_use]
    pub fn is_enum(&self) -> bool {
        matches!(self, Self::Enum(..))
    }

    /// Returns `true` if the evaluated type is [`Module`].
    ///
    /// [`Module`]: EvaluatedType::Module
    #[must_use]
    pub fn is_module(&self) -> bool {
        matches!(self, Self::Module(..))
    }

    /// Returns `true` if the evaluated type is [`ModelInstance`].
    ///
    /// [`ModelInstance`]: EvaluatedType::ModelInstance
    #[must_use]
    pub fn is_model_instance(&self) -> bool {
        matches!(self, Self::ModelInstance { .. })
    }
    /// Returns `true` if the evaluated type is [`Void`].
    ///
    /// [`Void`]: EvaluatedType::Void
    #[must_use]
    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    /// Returns `true` if the evaluated type is [`Unknown`].
    ///
    /// [`Unknown`]: EvaluatedType::Unknown
    #[must_use]
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    /// Returns `true` if the evaluated type is [`TraitInstance`].
    ///
    /// [`TraitInstance`]: EvaluatedType::TraitInstance
    #[must_use]
    pub fn is_trait_instance(&self) -> bool {
        matches!(self, Self::TraitInstance { .. })
    }

    /// Returns `true` if the evaluated type is [`Generic`].
    ///
    /// [`Generic`]: EvaluatedType::Generic
    #[must_use]
    pub fn is_generic(&self) -> bool {
        matches!(self, Self::Generic { .. })
    }

    /// Returns `true` if the evaluated type is [`Partial`].
    ///
    /// [`Partial`]: EvaluatedType::Partial
    #[must_use]
    pub fn is_partial(&self) -> bool {
        matches!(self, Self::Partial { .. })
    }

    pub(crate) fn contains_never(&self) -> bool {
        match self {
            EvaluatedType::Partial { types } => types.iter().any(|typ| typ.contains_never()),
            EvaluatedType::ModelInstance {
                generic_arguments, ..
            }
            | EvaluatedType::TraitInstance {
                generic_arguments, ..
            }
            | EvaluatedType::EnumInstance {
                generic_arguments, ..
            }
            | EvaluatedType::FunctionInstance {
                generic_arguments, ..
            }
            | EvaluatedType::MethodInstance {
                generic_arguments, ..
            } => generic_arguments
                .iter()
                .map(|(_, typ)| typ)
                .any(|typ| typ.contains_never()),
            EvaluatedType::FunctionExpressionInstance {
                params,
                return_type,
                generic_args,
                ..
            } => {
                params
                    .iter()
                    .any(|param| param.inferred_type.contains_never())
                    || return_type.contains_never()
                    || generic_args
                        .iter()
                        .map(|(_, typ)| typ)
                        .any(|typ| typ.contains_never())
            }
            EvaluatedType::OpaqueTypeInstance { .. } => false,
            EvaluatedType::Borrowed { base } => base.contains_never(),
            _ => false,
        }
    }
}

static mut RECURSION_DEPTH: u64 = 0;

/// Converts an intermediate type into an evaluation.
pub fn evaluate(
    typ: &IntermediateType,
    symboltable: &SymbolTable,
    // A map of the solutions for previously encountered generic types.
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
    // Error store from the standpoint, if it exists.
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
) -> EvaluatedType {
    let evaltype = match typ {
        IntermediateType::FunctionType {
            params,
            return_type,
            ..
        } => {
            let return_type = Box::new(
                return_type
                    .as_ref()
                    .map(|typ| evaluate(typ, symboltable, solved_generics, error_tracker))
                    .unwrap_or(EvaluatedType::Void),
            );
            EvaluatedType::FunctionExpressionInstance {
                is_async: false, // todo: always false (for now).
                params: params
                    .iter()
                    .map(|param| {
                        let mut param = param.clone();
                        param.inferred_type = param
                            .type_label
                            .as_ref()
                            .map(|typ| {
                                evaluate(typ, symboltable, solved_generics, &mut error_tracker)
                            })
                            .unwrap_or(EvaluatedType::Unknown);
                        param
                    })
                    .collect(),
                return_type,
                generic_args: solved_generics.cloned().unwrap_or(vec![]),
            }
        }
        IntermediateType::MemberType { .. } => EvaluatedType::Unknown, // todo
        IntermediateType::SimpleType {
            value,
            generic_args,
            span,
        } => {
            if symboltable
                .never_symbol
                .is_some_and(|never| never == *value)
            {
                unsafe {
                    RECURSION_DEPTH = 0;
                }
                return EvaluatedType::Never;
            }
            let value = symboltable.forward(*value);
            let typ = symboltable.get_forwarded(value).unwrap();
            let mut get_generics = |generic_params| {
                generate_generics_from_arguments(
                    generic_args,
                    generic_params,
                    error_tracker,
                    typ,
                    span,
                    symboltable,
                    solved_generics,
                )
            };
            match &typ.kind {
                SemanticSymbolKind::Module { .. } => EvaluatedType::Module(value),
                SemanticSymbolKind::Trait { generic_params, .. } => EvaluatedType::TraitInstance {
                    trait_: value,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::Model { generic_params, .. } => EvaluatedType::ModelInstance {
                    model: value,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::Enum { generic_params, .. } => EvaluatedType::EnumInstance {
                    enum_: value,
                    generic_arguments: get_generics(generic_params),
                },
                SemanticSymbolKind::TypeName {
                    generic_params,
                    value,
                    ..
                } => {
                    if unsafe { RECURSION_DEPTH } == 2000 {
                        add_error_if_possible(error_tracker, errors::infinite_type(*span));
                        unsafe {
                            RECURSION_DEPTH = 0;
                        }
                        return EvaluatedType::Unknown;
                    }
                    let mut generic_arguments = vec![];
                    let empty = vec![];
                    for generic_param in generic_params {
                        generic_arguments.push((
                            *generic_param,
                            coerce(
                                EvaluatedType::Generic {
                                    base: *generic_param,
                                },
                                *solved_generics.as_ref().unwrap_or(&&empty),
                            ),
                        ));
                    }
                    generic_arguments.append(&mut solved_generics.cloned().unwrap_or(empty));
                    evaluate(value, symboltable, Some(&generic_arguments), error_tracker)
                }
                SemanticSymbolKind::GenericParameter { .. } => {
                    // check if this type already has a solution.
                    if let Some(solved_generics) = solved_generics {
                        for (generic_parameter, evaluated_type) in solved_generics {
                            if *generic_parameter == value {
                                unsafe {
                                    RECURSION_DEPTH = 0;
                                }
                                return (*evaluated_type).clone();
                            }
                        }
                    }
                    EvaluatedType::Generic { base: value }
                }
                SemanticSymbolKind::UndeclaredValue => EvaluatedType::Unknown,
                _ => {
                    if error_tracker.is_some() {
                        let old = error_tracker.as_ref().unwrap().1;
                        error_tracker.as_mut().unwrap().1 =
                            typ.references.first().unwrap().module_path;
                        add_error_if_possible(
                            &mut error_tracker,
                            value_as_type(typ.name.clone(), *span),
                        );
                        error_tracker.as_mut().unwrap().1 = old;
                    }
                    EvaluatedType::Unknown
                }
            }
        }
        IntermediateType::This { meaning, span } => {
            // Generate a type that is equal to the enclosing model or trait.
            match meaning.clone() {
                Some(value) => {
                    let symbol = symboltable.get_forwarded(value).unwrap();
                    let generic_args = match &symbol.kind {
                        SemanticSymbolKind::Model { generic_params, .. }
                        | SemanticSymbolKind::Trait { generic_params, .. } => generic_params
                            .iter()
                            .map(|idx| IntermediateType::SimpleType {
                                value: *idx,
                                generic_args: vec![],
                                span: Span::default(),
                            })
                            .collect(),

                        _ => {
                            unsafe {
                                RECURSION_DEPTH = 0;
                            }
                            return EvaluatedType::Unknown;
                        }
                    };
                    let intermediate_type = IntermediateType::SimpleType {
                        value,
                        generic_args,
                        span: *span,
                    };
                    evaluate(
                        &intermediate_type,
                        symboltable,
                        solved_generics,
                        error_tracker,
                    )
                }
                None => EvaluatedType::Unknown,
            }
        }
        IntermediateType::BorrowedType { value, .. } => EvaluatedType::Borrowed {
            base: Box::new(evaluate(
                &value,
                symboltable,
                solved_generics,
                error_tracker,
            )),
        },
        IntermediateType::Placeholder => {
            unreachable!("Cannot evaluate placeholder intermediate type.")
        }
        _ => EvaluatedType::Unknown,
    };
    unsafe {
        RECURSION_DEPTH = 0;
    }
    evaltype
}

fn generate_generics_from_arguments(
    generic_args: &Vec<IntermediateType>,
    generic_params: &Vec<SymbolIndex>,
    mut error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    typ: &crate::SemanticSymbol,
    span: &Span,
    symboltable: &SymbolTable,
    solved_generics: Option<&Vec<(SymbolIndex, EvaluatedType)>>,
) -> Vec<(SymbolIndex, EvaluatedType)> {
    if generic_args.len() > 0 && generic_params.len() == 0 {
        add_error_if_possible(
            &mut error_tracker,
            errors::unexpected_generic_args(typ.name.clone(), *span),
        );
        vec![]
    } else {
        let args_len = generic_args.len();
        let param_len = generic_params.len();
        if args_len != param_len {
            add_error_if_possible(
                &mut error_tracker,
                errors::mismatched_generics(typ.name.clone(), param_len, args_len, *span),
            );
            vec![]
        } else {
            let mut generic_solutions = vec![];
            // Unify each parameter to argument.
            let mut i = 0;
            while i < param_len {
                // Convert each parameter symbol to an immediately evaluated generic param type.
                // which can then be unified with the argument
                // to determine the result value.
                let param_idx = generic_params[i];
                let intermediate_generic_type = IntermediateType::SimpleType {
                    value: param_idx,
                    generic_args: vec![],
                    span: Span::default(),
                };
                let generic_param_evaluated = evaluate(
                    &intermediate_generic_type,
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                );
                let argument_evaluated = evaluate(
                    &generic_args[i],
                    symboltable,
                    solved_generics,
                    &mut error_tracker,
                );
                let result_evaluated_type = match unify_types(
                    &generic_param_evaluated,
                    &argument_evaluated,
                    symboltable,
                    UnifyOptions::None,
                    None,
                ) {
                    Ok(value) => value,
                    Err(error) => {
                        for error in error {
                            add_error_if_possible(
                                &mut error_tracker,
                                TypeError {
                                    _type: error,
                                    span: generic_args[i].span(),
                                },
                            );
                        }
                        EvaluatedType::Unknown
                    }
                };
                generic_solutions.push((param_idx, result_evaluated_type));
                i += 1;
            }
            generic_solutions
        }
    }
}

fn add_error_if_possible(
    error_tracker: &mut Option<(&mut Vec<ProgramError>, PathIndex)>,
    error: TypeError,
) {
    if let Some((errors, path_idx)) = error_tracker {
        let error = ProgramError {
            offending_file: *path_idx,
            error_type: crate::ProgramErrorType::Typing(error),
        };
        if !errors.iter().any(|prior| *prior == error) {
            errors.push(error);
        }
    }
}
