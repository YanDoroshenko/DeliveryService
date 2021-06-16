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

CREATE TABLE delivery.location_override_rates (
    id uuid PRIMARY KEY,
    free_subtotal_threshold double,
    location_id text,
    lower_price_bound double,
    lower_subtotal_threshold double,
    starting_price double,
    subtotal_factor double,
    upper_price_bound double,
    upper_subtotal_threshold double,
    weight_interval double
);

CREATE MATERIALIZED VIEW delivery.location_rates_by_key AS SELECT * FROM delivery.location_override_rates WHERE location_id IS NOT NULL PRIMARY KEY (location_id, id);

CREATE TABLE delivery.base_distance_rates (
    id uuid PRIMARY KEY,
    distance_from double,
    distance_to double,
    free_subtotal_threshold double,
    lower_price_bound double,
    lower_subtotal_threshold double,
    starting_price double,
    subtotal_factor double,
    upper_price_bound double,
    upper_subtotal_threshold double,
    weight_interval double
);

CREATE TABLE delivery.state_override_rates (
    id uuid PRIMARY KEY,
    free_subtotal_threshold double,
    lower_price_bound double,
    lower_subtotal_threshold double,
    starting_price double,
    state_code text,
    subtotal_factor double,
    upper_price_bound double,
    upper_subtotal_threshold double,
    weight_interval double
);

CREATE MATERIALIZED VIEW delivery.state_override_rates_by_key AS SELECT * FROM delivery.state_override_rates WHERE state_code IS NOT NULL PRIMARY KEY (state_code, id);

