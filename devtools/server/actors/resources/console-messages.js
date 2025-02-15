/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

"use strict";

const { Cu } = require("chrome");
const {
  TYPES: { CONSOLE_MESSAGE },
} = require("devtools/server/actors/resources/index");
const DevToolsUtils = require("devtools/shared/DevToolsUtils");
const { WebConsoleUtils } = require("devtools/server/actors/webconsole/utils");
const {
  ConsoleAPIListener,
} = require("devtools/server/actors/webconsole/listeners/console-api");
const {
  createValueGrip,
  isArray,
} = require("devtools/server/actors/object/utils");

const { ObjectActor } = require("devtools/server/actors/object");
loader.lazyRequireGetter(
  this,
  "EnvironmentActor",
  "devtools/server/actors/environment",
  true
);

const listeners = new WeakMap();

/**
 * Start watching for all console messages related to a given Target Actor.
 * This will notify about existing console messages, but also the one created in future.
 *
 * @param TargetActor targetActor
 *        The target actor from which we should observe console messages
 * @param Object options
 *        Dictionary object with following attributes:
 *        - onAvailable: mandatory function
 *          This will be called for each resource.
 */
function watch(targetActor, { onAvailable }) {
  if (listeners.has(targetActor)) {
    throw new Error(
      "Already listening to console messages for this target actor"
    );
  }

  // The following code expects the ThreadActor to be instantiated, via:
  // prepareConsoleMessageForRemote > TabSources.getActorIdForInternalSourceId
  // The Thread Actor is instantiated via Target.attach, but we should
  // probably review this and only instantiate the actor instead of attaching the target.
  targetActor.attach();

  // Bug 1642297: Maybe we could merge ConsoleAPI Listener into this module?
  const onConsoleAPICall = message => {
    onAvailable([
      {
        resourceType: CONSOLE_MESSAGE,
        message: prepareConsoleMessageForRemote(targetActor, message),
      },
    ]);
  };

  // Create the consoleAPIListener
  // (and apply the filtering options defined in the target actor).
  const listener = new ConsoleAPIListener(
    targetActor.window,
    onConsoleAPICall,
    targetActor.consoleAPIListenerOptions
  );
  listener.init();
  listeners.set(targetActor, listener);

  // See `window` definition. It isn't always a DOM Window.
  const winStartTime =
    targetActor.window && targetActor.window.performance
      ? targetActor.window.performance.timing.navigationStart
      : 0;

  const cachedMessages = listener.getCachedMessages(!targetActor.isRootActor);
  const messages = [];
  // Filter out messages that came from a ServiceWorker but happened
  // before the page was requested.
  for (const message of cachedMessages) {
    if (
      message.innerID === "ServiceWorker" &&
      winStartTime > message.timeStamp
    ) {
      continue;
    }
    messages.push({
      resourceType: CONSOLE_MESSAGE,
      message: prepareConsoleMessageForRemote(targetActor, message),
    });
  }
  onAvailable(messages);
}

/**
 * Stop watching for console messages related to a given Target Actor.
 *
 * @param TargetActor targetActor
 *        The target actor from which we should stop observing console messages
 */
function unwatch(targetActor) {
  const listener = listeners.get(targetActor);
  if (!listener) {
    return;
  }
  listener.destroy();
  listeners.delete(targetActor);
}

function onLogPoint(targetActor, message) {
  const listener = listeners.get(targetActor);
  if (!listener) {
    targetActor._consoleActor.onConsoleAPICall(message);
    return;
    // Bug 1642296: Once we enable ConsoleMessage resource on the server, we should remove onConsoleAPICall
    // from the WebConsoleActor, and only support this codepath.
    // And we can then throw if no listener is found:
    // throw new Error("This target actor isn't listening to console messages");
  }
  listener.handler(message);
}

module.exports = {
  watch,
  unwatch,
  onLogPoint,
};

/**
 * Given a target actor and a source platform internal ID,
 * return the related SourceActor ID.

 * @param TargetActor targetActor
 *        The Target Actor from which this source originates.
 * @param String id
 *        Platform Source ID
 * @return String
 *         The SourceActor ID
 */
function getActorIdForInternalSourceId(targetActor, id) {
  const actor = targetActor.sources.getSourceActorByInternalSourceId(id);
  return actor ? actor.actorID : null;
}

/**
 * Create a grip for the given value.
 *
 * @param TargetActor targetActor
 *        The Target Actor from which this object originates.
 * @param mixed value
 *        The value you want to get a debuggee value for.
 * @param Number depth
 *        Depth of the object compared to the top level object,
 *        when we are inspecting nested attributes.
 * @return object
 */
function createValueGripForTarget(targetActor, value, depth = 0) {
  return createValueGrip(
    value,
    targetActor,
    createObjectGrip.bind(null, targetActor, depth)
  );
}

/**
 * Create and return an environment actor that corresponds to the provided
 * Debugger.Environment. This is a straightforward clone of the ThreadActor's
 * method except that it stores the environment actor in the web console
 * actor's pool.
 *
 * @param Debugger.Environment environment
 *        The lexical environment we want to extract.
 * @param TargetActor targetActor
 *        The Target Actor to use as parent actor.
 * @return The EnvironmentActor for |environment| or |undefined| for host
 *         functions or functions scoped to a non-debuggee global.
 */
function createEnvironmentActor(environment, targetActor) {
  if (!environment) {
    return undefined;
  }

  if (environment.actor) {
    return environment.actor;
  }

  const actor = new EnvironmentActor(environment, targetActor);
  targetActor.manage(actor);
  environment.actor = actor;

  return actor;
}

/**
 * Create a grip for the given object.
 *
 * @param TargetActor targetActor
 *        The Target Actor from which this object originates.
 * @param Number depth
 *        Depth of the object compared to the top level object,
 *        when we are inspecting nested attributes.
 * @param object object
 *        The object you want.
 * @param object pool
 *        A Pool where the new actor instance is added.
 * @param object
 *        The object grip.
 */
function createObjectGrip(targetActor, depth, object, pool) {
  let gripDepth = depth;
  const actor = new ObjectActor(
    object,
    {
      thread: targetActor.threadActor,
      getGripDepth: () => gripDepth,
      incrementGripDepth: () => gripDepth++,
      decrementGripDepth: () => gripDepth--,
      createValueGrip: v => createValueGripForTarget(targetActor, v, gripDepth),
      createEnvironmentActor: env => createEnvironmentActor(env, targetActor),
      sources: () =>
        DevToolsUtils.reportException(
          "WebConsoleActor",
          Error("sources not yet implemented")
        ),
    },
    targetActor.conn
  );
  pool.manage(actor);
  return actor.form();
}

function isObject(value) {
  return Object(value) === value;
}

/**
 * Make a debuggee value for the given value.
 *
 * @param TargetActor targetActor
 *        The Target Actor from which this object originates.
 * @param mixed value
 *        The value you want to get a debuggee value for.
 * @return object
 *         Debuggee value for |value|.
 */
function makeDebuggeeValue(targetActor, value) {
  if (isObject(value)) {
    try {
      const global = Cu.getGlobalForObject(value);
      const dbgGlobal = targetActor.dbg.makeGlobalObjectReference(global);
      return dbgGlobal.makeDebuggeeValue(value);
    } catch (ex) {
      // The above can throw an exception if value is not an actual object
      // or 'Object in compartment marked as invisible to Debugger'
    }
  }
  const dbgGlobal = targetActor.dbg.makeGlobalObjectReference(
    targetActor.window
  );
  return dbgGlobal.makeDebuggeeValue(value);
}

/**
 * Return the properties needed to display the appropriate table for a given
 * console.table call.
 * This function does a little more than creating an ObjectActor for the first
 * parameter of the message. When layout out the console table in the output, we want
 * to be able to look into sub-properties so the table can have a different layout (
 * for arrays of arrays, objects with objects properties, arrays of objects, …).
 * So here we need to retrieve the properties of the first parameter, and also all the
 * sub-properties we might need.
 *
 * @param {TargetActor} targetActor: The Target Actor from which this object originates.
 * @param {Object} result: The console.table message.
 * @returns {Object} An object containing the properties of the first argument of the
 *                   console.table call.
 */
function getConsoleTableMessageItems(targetActor, result) {
  if (
    !result ||
    !Array.isArray(result.arguments) ||
    result.arguments.length == 0
  ) {
    return null;
  }

  const [tableItemGrip] = result.arguments;
  const dataType = tableItemGrip.class;
  const needEntries = ["Map", "WeakMap", "Set", "WeakSet"].includes(dataType);
  const ignoreNonIndexedProperties = isArray(tableItemGrip);

  const tableItemActor = targetActor.actor(tableItemGrip.actor);
  if (!tableItemActor) {
    return null;
  }

  // Retrieve the properties (or entries for Set/Map) of the console table first arg.
  const iterator = needEntries
    ? tableItemActor.enumEntries()
    : tableItemActor.enumProperties({
        ignoreNonIndexedProperties,
      });
  const { ownProperties } = iterator.all();

  // The iterator returns a descriptor for each property, wherein the value could be
  // in one of those sub-property.
  const descriptorKeys = ["safeGetterValues", "getterValue", "value"];

  Object.values(ownProperties).forEach(desc => {
    if (typeof desc !== "undefined") {
      descriptorKeys.forEach(key => {
        if (desc && desc.hasOwnProperty(key)) {
          const grip = desc[key];

          // We need to load sub-properties as well to render the table in a nice way.
          const actor = grip && targetActor.actor(grip.actor);
          if (actor) {
            const res = actor
              .enumProperties({
                ignoreNonIndexedProperties: isArray(grip),
              })
              .all();
            if (res?.ownProperties) {
              desc[key].ownProperties = res.ownProperties;
            }
          }
        }
      });
    }
  });

  return ownProperties;
}

/**
 * Prepare a message from the console API to be sent to the remote Web Console
 * instance.
 *
 * @param TargetActor targetActor
 *        The related target actor
 * @param object message
 *        The original message received from console-api-log-event.
 * @return object
 *         The object that can be sent to the remote client.
 */
function prepareConsoleMessageForRemote(targetActor, message) {
  const result = WebConsoleUtils.cloneObject(message);

  result.workerType = WebConsoleUtils.getWorkerType(result) || "none";
  result.sourceId = getActorIdForInternalSourceId(targetActor, result.sourceId);

  delete result.wrappedJSObject;
  delete result.ID;
  delete result.innerID;
  delete result.consoleID;

  if (result.stacktrace) {
    result.stacktrace = result.stacktrace.map(frame => {
      return {
        ...frame,
        sourceId: getActorIdForInternalSourceId(targetActor, frame.sourceId),
      };
    });
  }

  result.arguments = (message.arguments || []).map(obj => {
    const dbgObj = makeDebuggeeValue(targetActor, obj);
    return createValueGripForTarget(targetActor, dbgObj);
  });

  result.styles = (message.styles || []).map(string => {
    return createValueGripForTarget(targetActor, string);
  });

  if (result.level === "table") {
    const tableItems = getConsoleTableMessageItems(targetActor, result);
    if (tableItems) {
      result.arguments[0].ownProperties = tableItems;
      result.arguments[0].preview = null;
    }

    // Only return the 2 first params.
    result.arguments = result.arguments.slice(0, 2);
  }

  result.category = message.category || "webdev";
  result.innerWindowID = message.innerID;

  return result;
}
