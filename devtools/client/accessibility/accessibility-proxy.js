/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

"use strict";

const Services = require("Services");

loader.lazyRequireGetter(
  this,
  "CombinedProgress",
  "devtools/client/accessibility/utils/audit",
  true
);

const {
  accessibility: { AUDIT_TYPE },
} = require("devtools/shared/constants");
const { FILTERS } = require("devtools/client/accessibility/constants");

/**
 * Component responsible for tracking all Accessibility fronts in parent and
 * content processes.
 */
class AccessibilityProxy {
  constructor(toolbox) {
    this.toolbox = toolbox;

    this._accessibilityWalkerFronts = new Set();
    this.lifecycleEvents = new Map();
    this.accessibilityEvents = new Map();
    this.supports = {};

    this.audit = this.audit.bind(this);
    this.disableAccessibility = this.disableAccessibility.bind(this);
    this.enableAccessibility = this.enableAccessibility.bind(this);
    this.getAccessibilityTreeRoot = this.getAccessibilityTreeRoot.bind(this);
    this.resetAccessiblity = this.resetAccessiblity.bind(this);
    this.startListeningForAccessibilityEvents = this.startListeningForAccessibilityEvents.bind(
      this
    );
    this.startListeningForLifecycleEvents = this.startListeningForLifecycleEvents.bind(
      this
    );
    this.startListeningForParentLifecycleEvents = this.startListeningForParentLifecycleEvents.bind(
      this
    );
    this.stopListeningForAccessibilityEvents = this.stopListeningForAccessibilityEvents.bind(
      this
    );
    this.stopListeningForLifecycleEvents = this.stopListeningForLifecycleEvents.bind(
      this
    );
    this.stopListeningForParentLifecycleEvents = this.stopListeningForParentLifecycleEvents.bind(
      this
    );
    this.highlightAccessible = this.highlightAccessible.bind(this);
    this.unhighlightAccessible = this.unhighlightAccessible.bind(this);
    this.onTargetAvailable = this.onTargetAvailable.bind(this);
    this.onTargetDestroyed = this.onTargetDestroyed.bind(this);
    this.onAccessibilityFrontAvailable = this.onAccessibilityFrontAvailable.bind(
      this
    );
    this.onAccessibilityFrontDestroyed = this.onAccessibilityFrontDestroyed.bind(
      this
    );
    this.onAccessibleWalkerFrontAvailable = this.onAccessibleWalkerFrontAvailable.bind(
      this
    );
    this.onAccessibleWalkerFrontDestroyed = this.onAccessibleWalkerFrontDestroyed.bind(
      this
    );
  }

  get enabled() {
    return this.accessibilityFront && this.accessibilityFront.enabled;
  }

  /**
   * Indicates whether the accessibility service is enabled.
   */
  get canBeEnabled() {
    return this.parentAccessibilityFront.canBeEnabled;
  }

  get currentTarget() {
    return this._currentTarget;
  }

  /**
   * Perform an audit for a given filter.
   *
   * @param  {String} filter
   *         Type of an audit to perform.
   * @param  {Function} onProgress
   *         Audit progress callback.
   *
   * @return {Promise}
   *         Resolves when the audit for every document, that each of the frame
   *         accessibility walkers traverse, completes.
   */
  async audit(filter, onProgress) {
    const types = filter === FILTERS.ALL ? Object.values(AUDIT_TYPE) : [filter];
    const totalFrames = this.toolbox.targetList.getAllTargets([
      this.toolbox.targetList.TYPES.FRAME,
    ]).length;
    const progress = new CombinedProgress({
      onProgress,
      totalFrames,
    });
    const audits = await this.withAllAccessibilityWalkerFronts(
      async accessibleWalkerFront =>
        accessibleWalkerFront.audit({
          types,
          onProgress: progress.onProgressForWalker.bind(
            progress,
            accessibleWalkerFront
          ),
        })
    );

    // Accumulate all audits into a single structure.
    const combinedAudit = { ancestries: [] };
    for (const audit of audits) {
      // If any of the audits resulted in an error, no need to continue.
      if (audit.error) {
        return audit;
      }

      combinedAudit.ancestries.push(...audit.ancestries);
    }

    return combinedAudit;
  }

  async disableAccessibility() {
    // Accessibility service is shut down using the parent accessibility front.
    // That, in turn, shuts down accessibility service in all content processes.
    // We need to wait until that happens to be sure platform  accessibility is
    // fully disabled.
    const disabled = this.accessibilityFront.once("shutdown");
    await this.parentAccessibilityFront.disable();
    await disabled;
  }

  async enableAccessibility() {
    // Accessibility service is initialized using the parent accessibility
    // front. That, in turn, initializes accessibility service in all content
    // processes. We need to wait until that happens to be sure platform
    // accessibility is fully enabled.
    const enabled = this.accessibilityFront.once("init");
    await this.parentAccessibilityFront.enable();
    await enabled;
  }

  /**
   * Return the topmost level accessibility walker to be used as the root of
   * the accessibility tree view.
   *
   * @return {Object}
   *         Topmost accessibility walker.
   */
  getAccessibilityTreeRoot() {
    return this.accessibilityFront.accessibleWalkerFront;
  }

  /**
   * Look up accessibility fronts (get an existing one or create a new one) for
   * all existing target fronts and run a task with each one of them.
   * @param {Function} task
   *        Function to execute with each accessiblity front.
   */
  async withAllAccessibilityFronts(taskFn) {
    const accessibilityFronts = await this.toolbox.targetList.getAllFronts(
      this.toolbox.targetList.TYPES.FRAME,
      "accessibility"
    );
    const tasks = [];
    for (const accessibilityFront of accessibilityFronts) {
      tasks.push(taskFn(accessibilityFront));
    }

    return Promise.all(tasks);
  }

  /**
   * Look up accessibility walker fronts (get an existing one or create a new
   * one using accessibility front) for all existing target fronts and run a
   * task with each one of them.
   * @param {Function} task
   *        Function to execute with each accessiblity walker front.
   */
  withAllAccessibilityWalkerFronts(taskFn) {
    return this.withAllAccessibilityFronts(async accessibilityFront => {
      if (!accessibilityFront.accessibleWalkerFront) {
        await accessibilityFront.bootstrap();
      }

      return taskFn(accessibilityFront.accessibleWalkerFront);
    });
  }

  /**
   * Start picking and add walker listeners.
   * @param  {Boolean} doFocus
   *         If true, move keyboard focus into content.
   */
  pick(doFocus, onHovered, onPicked, onPreviewed, onCanceled) {
    return this.withAllAccessibilityWalkerFronts(
      async accessibleWalkerFront => {
        this.startListening(accessibleWalkerFront, {
          events: {
            "picker-accessible-hovered": onHovered,
            "picker-accessible-picked": onPicked,
            "picker-accessible-previewed": onPreviewed,
            "picker-accessible-canceled": onCanceled,
          },
          // Only register listeners once (for top level), no need to register
          // them for all walkers again and again.
          register: accessibleWalkerFront.targetFront.isTopLevel,
        });
        await accessibleWalkerFront.pick(
          // Only pass doFocus to the top level accessibility walker front.
          doFocus && accessibleWalkerFront.targetFront.isTopLevel
        );
      }
    );
  }

  /**
   * Stop picking and remove all walker listeners.
   */
  cancelPick(onHovered, onPicked, onPreviewed, onCanceled) {
    return this.withAllAccessibilityWalkerFronts(
      async accessibleWalkerFront => {
        await accessibleWalkerFront.cancelPick();
        this.stopListening(accessibleWalkerFront, {
          events: {
            "picker-accessible-hovered": onHovered,
            "picker-accessible-picked": onPicked,
            "picker-accessible-previewed": onPreviewed,
            "picker-accessible-canceled": onCanceled,
          },
          // Only unregister listeners once (for top level), no need to
          // unregister them for all walkers again and again.
          unregister: accessibleWalkerFront.targetFront.isTopLevel,
        });
      }
    );
  }

  async resetAccessiblity() {
    const { enabled } = this.accessibilityFront;
    const { canBeEnabled, canBeDisabled } =
      this.parentAccessibilityFront || this.accessibilityFront;
    return { enabled, canBeDisabled, canBeEnabled };
  }

  startListening(front, { events, register = false } = {}) {
    for (const [type, listener] of Object.entries(events)) {
      front.on(type, listener);
      if (register) {
        this.registerEvent(front, type, listener);
      }
    }
  }

  stopListening(front, { events, unregister = false } = {}) {
    for (const [type, listener] of Object.entries(events)) {
      front.off(type, listener);
      if (unregister) {
        this.unregisterEvent(front, type, listener);
      }
    }
  }

  startListeningForAccessibilityEvents(events) {
    for (const accessibleWalkerFront of this._accessibilityWalkerFronts.values()) {
      this.startListening(accessibleWalkerFront, {
        events,
        // Only register listeners once (for top level), no need to register
        // them for all walkers again and again.
        register: accessibleWalkerFront.targetFront.isTopLevel,
      });
    }
  }

  stopListeningForAccessibilityEvents(events) {
    for (const accessibleWalkerFront of this._accessibilityWalkerFronts.values()) {
      this.stopListening(accessibleWalkerFront, {
        events,
        // Only unregister listeners once (for top level), no need to unregister
        // them for all walkers again and again.
        unregister: accessibleWalkerFront.targetFront.isTopLevel,
      });
    }
  }

  startListeningForLifecycleEvents(events) {
    this.startListening(this.accessibilityFront, { events, register: true });
  }

  stopListeningForLifecycleEvents(events) {
    this.stopListening(this.accessibilityFront, { events, unregister: true });
  }

  startListeningForParentLifecycleEvents(events) {
    this.startListening(this.parentAccessibilityFront, {
      events,
      register: false,
    });
  }

  stopListeningForParentLifecycleEvents(events) {
    this.stopListening(this.parentAccessibilityFront, {
      events,
      unregister: false,
    });
  }

  highlightAccessible(accessibleFront, options) {
    if (!accessibleFront) {
      return;
    }

    const accessibleWalkerFront = accessibleFront.getParent();
    if (!accessibleWalkerFront) {
      return;
    }

    accessibleWalkerFront
      .highlightAccessible(accessibleFront, options)
      .catch(error => {
        // Only report an error where there's still a toolbox. Ignore cases
        // where toolbox is already destroyed.
        if (this.toolbox) {
          console.error(error);
        }
      });
  }

  unhighlightAccessible(accessibleFront) {
    if (!accessibleFront) {
      return;
    }

    const accessibleWalkerFront = accessibleFront.getParent();
    if (!accessibleWalkerFront) {
      return;
    }

    accessibleWalkerFront.unhighlight().catch(error => {
      // Only report an error where there's still a toolbox. Ignore cases
      // where toolbox is already destroyed.
      if (this.toolbox) {
        console.error(error);
      }
    });
  }

  /**
   * Part of the proxy initialization only needs to be done when the accessibility panel starts.
   * To avoid performance issues, the panel will explicitly call this method every time a new
   * target becomes available.
   */
  async initializeProxyForPanel(targetFront) {
    await this.onTargetAvailable({ targetFront });

    // No need to retrieve parent accessibility front since root front does not
    // change.
    if (!this.parentAccessibilityFront) {
      this.parentAccessibilityFront = await this._currentTarget.client.mainRoot.getFront(
        "parentaccessibility"
      );
    }

    this.simulatorFront = this.accessibilityFront.simulatorFront;
    if (this.simulatorFront) {
      this.simulate = types => this.simulatorFront.simulate({ types });
    } else {
      this.simulate = null;
    }

    // Move accessibility front lifecycle event listeners to a new top level
    // front.
    for (const [type, listeners] of this.lifecycleEvents.entries()) {
      for (const listener of listeners.values()) {
        this.accessibilityFront.on(type, listener);
      }
    }
  }

  async initialize() {
    try {
      await this.toolbox.targetList.watchTargets(
        [this.toolbox.targetList.TYPES.FRAME],
        this.onTargetAvailable,
        this.onTargetDestroyed
      );
      // Bug 1602075: auto init feature definition is used for an experiment to
      // determine if we can automatically enable accessibility panel when it
      // opens.
      this.supports.autoInit = Services.prefs.getBoolPref(
        "devtools.accessibility.auto-init.enabled",
        false
      );

      return true;
    } catch (e) {
      // toolbox may be destroyed during this step.
      return false;
    }
  }

  destroy() {
    this.toolbox.targetList.unwatchTargets(
      [this.toolbox.targetList.TYPES.FRAME],
      this.onTargetAvailable,
      this.onTargetDestroyed
    );

    this.lifecycleEvents.clear();
    this.accessibilityEvents.clear();

    this.accessibilityFront = null;
    this.parentAccessibilityFront = null;
    this.simulatorFront = null;
    this.simulate = null;
    this.toolbox = null;
  }

  _getEvents(front) {
    return front.typeName === "accessiblewalker"
      ? this.accessibilityEvents
      : this.lifecycleEvents;
  }

  registerEvent(front, type, listener) {
    const events = this._getEvents(front);
    if (events.has(type)) {
      events.get(type).add(listener);
    } else {
      events.set(type, new Set([listener]));
    }
  }

  unregisterEvent(front, type, listener) {
    const events = this._getEvents(front);
    if (!events.has(type)) {
      return;
    }

    const listeners = events.get(type);
    if (listeners.has(listener)) {
      listeners.delete(listener);
    }

    if (!listeners.size) {
      events.delete(type);
    }
  }

  onAccessibilityFrontAvailable(accessibilityFront) {
    accessibilityFront.watchFronts(
      "accessiblewalker",
      this.onAccessibleWalkerFrontAvailable,
      this.onAccessibleWalkerFrontDestroyed
    );
  }

  onAccessibilityFrontDestroyed(accessibilityFront) {
    accessibilityFront.unwatchFronts(
      "accessiblewalker",
      this.onAccessibleWalkerFrontAvailable,
      this.onAccessibleWalkerFrontDestroyed
    );
  }

  onAccessibleWalkerFrontAvailable(accessibleWalkerFront) {
    this._accessibilityWalkerFronts.add(accessibleWalkerFront);
    // Apply all existing accessible walker front event listeners to the new
    // front.
    for (const [type, listeners] of this.accessibilityEvents.entries()) {
      for (const listener of listeners) {
        accessibleWalkerFront.on(type, listener);
      }
    }
  }

  onAccessibleWalkerFrontDestroyed(accessibleWalkerFront) {
    this._accessibilityWalkerFronts.delete(accessibleWalkerFront);
    // Remove all existing accessible walker front event listeners from the
    // destroyed front.
    for (const [type, listeners] of this.accessibilityEvents.entries()) {
      for (const listener of listeners) {
        accessibleWalkerFront.off(type, listener);
      }
    }
  }

  async onTargetAvailable({ targetFront }) {
    targetFront.watchFronts(
      "accessibility",
      this.onAccessibilityFrontAvailable,
      this.onAccessibilityFrontDestroyed
    );

    if (!targetFront.isTopLevel) {
      return null;
    }

    if (this._updatePromise && this._currentTarget === targetFront) {
      return this._updatePromise;
    }

    this._currentTarget = targetFront;
    this._accessibilityWalkerFronts.clear();

    this._updatePromise = (async () => {
      this.accessibilityFront = await this._currentTarget.getFront(
        "accessibility"
      );
      // Finalize accessibility front initialization. See accessibility front
      // bootstrap method description.
      await this.accessibilityFront.bootstrap();
      // To add a check for backward compatibility add something similar to the
      // example below:
      //
      // [this.supports.simulation] = await Promise.all([
      //   // Please specify the version of Firefox when the feature was added.
      //   this._currentTarget.actorHasMethod("accessibility", "getSimulator"),
      // ]);
    })();

    return this._updatePromise;
  }

  async onTargetDestroyed({ targetFront }) {
    targetFront.unwatchFronts(
      "accessibility",
      this.onAccessibilityFrontAvailable,
      this.onAccessibilityFrontDestroyed
    );
  }
}

exports.AccessibilityProxy = AccessibilityProxy;
