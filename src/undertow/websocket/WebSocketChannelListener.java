package undertow.websocket;

import clojure.lang.*;
import io.undertow.websockets.core.*;
import org.xnio.Pooled;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.Arrays;

// TODO: Document purpose of the class

public class WebSocketChannelListener extends AbstractReceiveListener implements OnOpenListener {
  private final IFn onOpen;
  private final IFn onMessage;
  private final IFn onClose;
  private final IFn onError;

  private static final Keyword k_callback = RT.keyword(null, "callback");
  private static final Keyword k_channel = RT.keyword(null, "channel");
  private static final Keyword k_text = RT.keyword(null, "text");
  private static final Keyword k_data = RT.keyword(null, "data");
  private static final Keyword k_onMessage = RT.keyword(null, "on-message");

  public WebSocketChannelListener(ILookup config) {
    this.onOpen = (IFn) config.valAt(RT.keyword(null, "on-open"));
    this.onMessage = (IFn) config.valAt(RT.keyword(null, "on-message"));
    this.onClose = (IFn) config.valAt(RT.keyword(null, "on-close"));
    this.onError = (IFn) config.valAt(RT.keyword(null, "on-error"));
  }

  @Override
  public void onOpen(WebSocketChannel channel) {
    if (onOpen != null) {
      onOpen.invoke(RT.map(k_callback, RT.keyword(null, "on-open"),
                           k_channel, channel));
    }
  }

  @Override
  protected void onFullTextMessage(WebSocketChannel channel,
                                   BufferedTextMessage text) throws IOException {
    if (this.onMessage == null)
      super.onFullTextMessage(channel, text);
    else
      onMessage.invoke(RT.map(k_callback, k_onMessage,
                              k_channel, channel,
                              k_text, text.getData()));
  }

  @Override
  protected void onFullBinaryMessage(WebSocketChannel channel,
                                     BufferedBinaryMessage binary) throws IOException {
    if (this.onMessage == null)
      super.onFullBinaryMessage(channel, binary);
    else {
      @SuppressWarnings("deprecation")
      Pooled<ByteBuffer[]> pooled = binary.getData();
      byte[] buffer = WebSockets.mergeBuffers(pooled.getResource()).array();
      byte[] data = Arrays.copyOf(buffer, buffer.length);
      pooled.free();
      onMessage.invoke(RT.map(k_callback, k_onMessage,
                              k_channel, channel,
                              k_data, data));
    }
  }

  @Override
  protected void onCloseMessage(CloseMessage cm,
                                WebSocketChannel channel) {
    if (this.onError == null)
      super.onCloseMessage(cm, channel);
    else
      onClose.invoke(RT.map(k_callback, RT.keyword(null, "on-close"),
                            k_channel, channel,
                            RT.keyword(null, "code"), cm.getCode(),
                            RT.keyword(null, "reason"), cm.getReason()));
  }

  @Override
  protected void onError(WebSocketChannel channel,
                         Throwable error) {
    if (this.onError == null)
      super.onError(channel, error);
    else
      this.onError.invoke(RT.map(k_callback, RT.keyword(null, "on-error"),
                                 k_channel, channel,
                                 RT.keyword(null, "error"), error));
  }

}
