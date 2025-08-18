open struct
type hidden = {
    _hidden_val: float;
}
end

type exported = {
    _hidden : hidden;
    outer : float;
}

let create_exported () : exported =
    let hidden = {
        _hidden_val = 0.;
    } in
    {
        _hidden = hidden;
        outer = 0.;
    }
