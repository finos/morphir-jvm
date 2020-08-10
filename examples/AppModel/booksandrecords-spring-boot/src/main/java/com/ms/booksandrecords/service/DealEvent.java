/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


package com.ms.booksandrecords.service;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.ms.booksandrecords.service.dealevent.DealClosed;
import com.ms.booksandrecords.service.dealevent.DealNotFound;

//Add this property to the json to show this data in the output
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type")
@JsonSubTypes({
        @JsonSubTypes.Type(value = DealClosed.class, name = "dealClosed"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "dealNotFound"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "dealOpened"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "duplicateDeal"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "invalidPrice"),
        @JsonSubTypes.Type(value = DealNotFound.class, name = "invalidQuantity")
})
public abstract class DealEvent {
}