sqor_msg_publisher
==================

Generic Standalon Message Publisher/Router. It accepts messages then publish to Rabbitmq. Prior to publishing, it may process/transform the messages in 3 stages: enrich, transform, filter the messages depending on configuration.
