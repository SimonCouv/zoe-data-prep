def na_or_value(value):
    if value == '':
        return 'na'
    return value

def na_compare(value1, value2):
    lv1 = value1.lower()
    lv2 = value2.lower()
    if lv1 == 'na' and lv2 in ('', 'na'):
        return True
    return lv1 == lv2

def check_row(exp_ds, exp_index, act_ds, act_index, keys, custom_checks):
    disparities = None
    for k in keys:
        if isinstance(k, str):
            kexp, kact = k, k
        else:
            kexp, kact = k
        if kexp in custom_checks:
            check = custom_checks[kexp]
        else:
            check = na_compare
        exp_value = exp_ds.field_by_name(kexp)[exp_index]
        act_value = act_ds.field_by_name(kact)[act_index]
        if not check(exp_value, act_value):
            if disparities is None:
                disparities = list()
            if kexp == kact:
                disparities.append(f'{kexp} : {str(na_or_value(exp_value))} / {str(na_or_value(act_value))}')
            else:
                disparities.append(f'{kexp} / {kact} : {str(na_or_value(exp_value))} / {str(na_or_value(act_value))}')
    return disparities