const closeSvg = `
<svg width="16"  height="16"xmlns="http://www.w3.org/2000/svg" fill="none"><g class="fills"><rect rx="0" ry="0" width="16" height="16" class="frame-background"/></g><g class="frame-children"><path d="M11.997 3.997 8 8l-3.997 4.003m-.006-8L8 8l4.003 3.997" class="fills"/><g class="strokes"><path d="M11.997 3.997 8 8l-3.997 4.003m-.006-8L8 8l4.003 3.997" style="fill: none; stroke-width: 1; stroke: rgb(143, 157, 163); stroke-opacity: 1; stroke-linecap: round;" class="stroke-shape"/></g></g></svg>`;

import type { Theme } from '@penpot/plugin-types';
import { dragHandler } from '../drag-handler.js';
import modalCss from './plugin.modal.css?inline';
import { resizeModal } from '../create-modal.js';

const MIN_Z_INDEX = 3;

export class PluginModalElement extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  wrapper = document.createElement('div');
  #inner = document.createElement('div');
  #dragEvents: ReturnType<typeof dragHandler> | null = null;

  setTheme(theme: Theme) {
    if (this.wrapper) {
      this.wrapper.setAttribute('data-theme', theme);
    }
  }

  resize(width: number, height: number) {
    if (this.wrapper) {
      resizeModal(this, width, height);
    }
  }

  disconnectedCallback() {
    this.#dragEvents?.();
  }

  calculateZIndex() {
    const modals = document.querySelectorAll<HTMLElement>('plugin-modal');

    const zIndexModals = Array.from(modals)
      .filter((modal) => modal !== this)
      .map((modal) => {
        return Number(modal.style.zIndex);
      });

    const maxZIndex = Math.max(...zIndexModals, MIN_Z_INDEX);

    this.style.zIndex = (maxZIndex + 1).toString();
  }

  connectedCallback() {
    const title = this.getAttribute('title');
    const iframeSrc = this.getAttribute('iframe-src');
    const allowDownloads = this.getAttribute('allow-downloads') || false;

    if (!title || !iframeSrc) {
      throw new Error('title and iframe-src attributes are required');
    }

    if (!this.shadowRoot) {
      throw new Error('Error creating shadow root');
    }

    this.#inner.classList.add('inner');

    this.wrapper.classList.add('wrapper');
    this.wrapper.style.maxInlineSize = '90vw';
    this.wrapper.style.maxBlockSize = '90vh';

    // move modal to the top
    this.#dragEvents = dragHandler(this.#inner, this.wrapper, () => {
      this.calculateZIndex();
    });

    const header = document.createElement('div');
    header.classList.add('header');

    const h1 = document.createElement('h1');
    h1.textContent = title;

    header.appendChild(h1);

    // const closeButton = document.createElement('button');
    // closeButton.setAttribute('type', 'button');
    // closeButton.innerHTML = `<div class="close">${closeSvg}</div>`;
    // closeButton.addEventListener('click', () => {
    //   if (!this.shadowRoot) {
    //     return;
    //   }
    //
    //   this.shadowRoot.dispatchEvent(
    //     new CustomEvent('close', {
    //       composed: true,
    //       bubbles: true,
    //     }),
    //   );
    // });
    // header.appendChild(closeButton);

    const minimizeSVG = `<svg width="18" height="18" viewBox="0 0 18 18" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M4.5 9H13.5" stroke="#292D32" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/></svg>`;
    const maximizeSVG = `<svg width="18" height="18" viewBox="0 0 18 18" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M1.5 7.485V6.75C1.5 3 3 1.5 6.75 1.5H11.25C15 1.5 16.5 3 16.5 6.75V11.25C16.5 15 15 16.5 11.25 16.5H10.5" stroke="#292D32" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/><path d="M9.75 8.25002L13.5075 4.48502H10.5" stroke="#292D32" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/><path d="M13.5078 4.48502V7.49252" stroke="#292D32" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/><path d="M8.25 12.1125V14.1375C8.25 15.825 7.575 16.5 5.8875 16.5H3.8625C2.175 16.5 1.5 15.825 1.5 14.1375V12.1125C1.5 10.425 2.175 9.75 3.8625 9.75H5.8875C7.575 9.75 8.25 10.425 8.25 12.1125Z" stroke="#292D32" stroke-width="1.125" stroke-linecap="round" stroke-linejoin="round"/></svg>`;

    const minimizeButton = document.createElement('button');
    minimizeButton.setAttribute('type', 'button');
    minimizeButton.innerHTML = `<div class="minimize">${minimizeSVG}</div>`;

    minimizeButton.addEventListener('click', () => {
      if (!this.shadowRoot) {
        return;
      }

      this.shadowRoot.dispatchEvent(
          new CustomEvent('minimize', {
            composed: true,
            bubbles: true,
          }),
      );
    });
    header.appendChild(minimizeButton);

    const iframe = document.createElement('iframe');
    iframe.src = iframeSrc;
    iframe.allow = '';
    iframe.sandbox.add(
      'allow-scripts',
      'allow-forms',
      'allow-modals',
      'allow-popups',
      'allow-popups-to-escape-sandbox',
      'allow-storage-access-by-user-activation',
      'allow-same-origin',
      'allow-top-navigation-by-user-activation'
    );

    if (allowDownloads) {
      iframe.sandbox.add('allow-downloads');
    }

    iframe.addEventListener('load', () => {
      this.shadowRoot?.dispatchEvent(
        new CustomEvent('load', {
          composed: true,
          bubbles: true,
        }),
      );
    });

    this.addEventListener('message', (e: Event) => {
      if (!iframe.contentWindow) {
        return;
      }

      iframe.contentWindow.postMessage((e as CustomEvent).detail, '*');
    });

    this.shadowRoot.appendChild(this.wrapper);

    this.wrapper.appendChild(this.#inner);
    this.#inner.appendChild(header);
    this.#inner.appendChild(iframe);

    const style = document.createElement('style');
    style.textContent = modalCss;

    this.shadowRoot.appendChild(style);

    this.calculateZIndex();

    let isMinimized   = false;
    let prevHeight    = "536px";
    let prevMinHeight = "536px";
    minimizeButton.addEventListener("click", () => {
      let wrapperEl = this.shadowRoot?.querySelector(".wrapper");

      if (!wrapperEl && this.shadowRoot) {
        wrapperEl = this.shadowRoot.host;
      }

      if (!wrapperEl) {
        console.warn("Could not find .wrapper to minimize!");
        return;
      }

      // On first click: store and collapse
      if (!isMinimized) {
        prevHeight    = wrapperEl.style.height;
        prevMinHeight = wrapperEl.style.minHeight;

        wrapperEl.style.height    = `40px`;
        wrapperEl.style.minHeight = `40px`;

        minimizeButton.innerHTML = `<div class="maximize">${maximizeSVG}</div>`;
      }

      else {
        wrapperEl.style.height    = prevHeight;
        wrapperEl.style.minHeight = prevMinHeight;
        minimizeButton.innerHTML = `<div class="minimize">${minimizeSVG}</div>`;
      }

      isMinimized = !isMinimized;
    });

  }

  size() {
    const width = Number(this.wrapper.style.width.replace('px', '') || '300');
    const height = Number(this.wrapper.style.height.replace('px', '') || '400');

    return { width, height };
  }
}

customElements.define('plugin-modal', PluginModalElement);
