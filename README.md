[![Build Status](https://travis-ci.org/synlay/sf_client.svg?branch=develop)](https://travis-ci.org/synlay/sf_client) 
[![Coverage Status](https://coveralls.io/repos/github/synlay/sf_client/badge.svg?branch=develop)](https://coveralls.io/github/synlay/sf_client?branch=develop)
[![Hex.pm](https://img.shields.io/hexpm/v/sf_client.svg)](https://hex.pm/packages/sf_client) 
[![GitHub license](https://img.shields.io/github/license/synlay/sf_client.svg)](https://github.com/synlay/sf_client)

sf_client
=====

This library acts as an Erlang/OTP client for Salesforce. The library focuses on making
unidirectional outbound calls in order to map internal data models to external `sObjects` in Saleforce.
Future versions will probably also include inbound connections and other functionality like calling Apex functions.

## Usage

The following examples will illustrate how actual `sf_client` calls might look like:

```erlang
Model = #{id => 42, full_name => "Jon Snow"}
{ok, SObjectID} = sf_client:create(customer, Model),
{ok, SObjectID} = sf_client:get_sobject_id_by_model(customer, Model),
ok = sf_client:update(customer, SObjectID, Model#{full_name := "Snow, Jon"}),
ok = sf_client:delete(customer, SObjectID),
{error, not_found} = sf_client:get_sobject_id_by_model(customer, Model).
```
You can find some more examples in the test folder.

## Example

Following the previous usage example, your application configuration would contain on ore more `model` to `sObject`
mappings:

#### app.config:
```erlang
{sf_client, [
    ...
    ,{sobjects_mapping, #{
        customer => myapp_sf_customer_outbound
    }}
    ...
]}
```

`customer` identifies your internal model type and `myapp_sf_customer_outbound` is the module
which implements the `sf_client_sobjects_mapping_behaviour` behaviour. This behaviour defines all necessary functions
to be able to map your model to a particular `sObject`.

A module which implements the `sf_client_sobjects_mapping_behaviour` behaviour will typically look similar to this:

#### myapp_sf_customer_outbound.erl:
```erlang
-module(myapp_sf_customer_outbound).

-behaviour(sf_client_sobjects_mapping_behaviour).

...

-spec new_sobject_from_model(Model :: any()) -> DbSObject :: [{Key :: binary(), Value :: term()}].
new_sobject_from_model(Model) ->
    [
        {<<"Name">>, list_to_binary(Model#{full_name}),
        {<<"Type">>, <<"Customer">>},
        {<<"External_ID__c">>, integer_to_binary(Model#{id})}
    ].


-spec sobject_external_id_attribute_name() -> IdAttributeName :: binary().
sobject_external_id_attribute_name() ->
    <<"External_ID__c">>.


-spec model_id(Model :: any()) -> ModelId :: binary().
model_id(Model) ->
    integer_to_binary(Model#{id}).


-spec sobject_table_name() -> SObjectTableName :: binary().
sobject_table_name() ->
    <<"Account">>.


-spec to_string(Model :: any()) -> ModelTextRepresentation :: string().
to_string(Model) ->
    Model#{full_name}.

```

## Configuration

The client interacts with the Force.com REST API using the [Username-Password OAuth 2.0 Authentication Flow](https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/intro_understanding_username_password_oauth_flow.htm),
therefore it's necessary to create and set up an application as a connected app in your Salesforce organization. See [Defining Connected Apps](https://developer.salesforce.com/docs/atlas.en-us.api_rest.meta/api_rest/intro_defining_remote_access_applications.htm)
for more information.

### Configuration through environment

Additional to the default configuration through the applications config, it's possible to configure the client through
environment variables. Environment variables will always take a higher precedence.

| Option | Description |
| --- | --- |
| `SF_REST_API_VERSION` | API version to use |
| `SF_REST_API_ENDPOINT` | API endpoint to send requests to |
| `SF_CREDENTIALS_CLIENT_ID` | The Consumer Key from the connected app definition |
| `SF_CREDENTIALS_CLIENT_SECRET` | The Consumer Secret from the connected app definition |
| `SF_CREDENTIALS_USERNAME` | End-user’s username |
| `SF_CREDENTIALS_PASSWORD` | End-user’s password - Note: You must append the user’s security token to their password |
| `SF_ACCESS_TOKEN_EXPIRY` | Time in seconds after the client should try to retrieve a new access token |
