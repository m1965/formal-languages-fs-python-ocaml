def evaluate_adt(adt):
    match adt:
        case ('NUMBER', value):
            return float(value)
        case ('IDENTIFIER', var_name):
            if var_name not in Names2Values:
                print(f"Error: variable '{var_name}' is undefined.")
                return 0.0
            return Names2Values[var_name]
        case ((':=', ';'), ('IDENTIFIER', var_name), expr):
            Names2Values[var_name] = evaluate_adt(expr)
        case (';', expr):
            print(evaluate_adt(expr))
        case (('+' | '-' | '*' | '/') as op, left, right):
            val_left  = evaluate_adt(left )
            val_right = evaluate_adt(right)
            match op:
                case '+': return val_left + val_right
                case '-': return val_left - val_right
                case '*': return val_left * val_right
                case '/': return val_left / val_right
        case _:
            raise ValueError(f"Unknown ADT structure: {adt}")
