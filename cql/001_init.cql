CREATE TABLE delivery.postal_code_override_rates (
    id uuid PRIMARY KEY,
    free_subtotal_threshold double,
    lower_price_bound double,
    lower_subtotal_threshold double,
    postal_code text,
    starting_price double,
    subtotal_factor double,
    upper_price_bound double,
    upper_subtotal_threshold double,
    weight_interval double
);

CREATE MATERIALIZED VIEW delivery.postal_code_rates_by_key AS SELECT * FROM delivery.postal_code_override_rates WHERE postal_code IS NOT NULL PRIMARY KEY (postal_code, id);
