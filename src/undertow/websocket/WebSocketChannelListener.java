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
  private final Object context;

  private static final Keyword k_channel = RT.keyword(null, "channel");
  private static final Keyword k_message = RT.keyword(null, "message");
  private static final Keyword k_context = RT.keyword(null, "context");

  public WebSocketChannelListener(ILookup config) {
    this.onOpen = (IFn) config.valAt(RT.keyword(null, "on-open"));
    this.onMessage = (IFn) config.valAt(RT.keyword(null, "on-message"));
    this.onClose = (IFn) config.valAt(RT.keyword(null, "on-close"));
    this.onError = (IFn) config.valAt(RT.keyword(null, "on-error"));
    this.context = config.valAt(k_context);
  }

  @Override
  protected void onError(WebSocketChannel channel,
                         Throwable error) {
    if (this.onError == null)
      super.onError(channel, error);
    else
      this.onError.invoke(RT.map(k_channel, channel,
                                 k_context, context,
                                 RT.keyword(null, "error"), error));
  }

  @Override
  protected void onFullTextMessage(WebSocketChannel channel,
                                   BufferedTextMessage message) throws IOException {
    if (this.onMessage == null)
      super.onFullTextMessage(channel, message);
    else
      onMessage.invoke(RT.map(k_channel, channel,
                              k_message, message.getData(),
                              k_context, context));
  }

  @Override
  protected void onFullBinaryMessage(WebSocketChannel channel,
                                     BufferedBinaryMessage message) throws IOException {
    if (this.onMessage == null)
      super.onFullBinaryMessage(channel, message);
    else {
      @SuppressWarnings("deprecation")
      Pooled<ByteBuffer[]> data = message.getData();
      byte[] buffer = WebSockets.mergeBuffers(data.getResource()).array();
      byte[] bytes = Arrays.copyOf(buffer, buffer.length);
      data.free();
      onMessage.invoke(RT.map(k_channel, channel,
                              k_message, bytes,
                              k_context, context));
    }
  }

  @Override
  protected void onCloseMessage(CloseMessage cm,
                                WebSocketChannel channel) {
    if (this.onError == null)
      super.onCloseMessage(cm, channel);
    else
      onClose.invoke(RT.map(k_channel, channel,
                            RT.keyword(null, "code"), cm.getCode(),
                            RT.keyword(null, "reason"), cm.getReason(),
                            k_context, context));
  }

  @Override
  public void onOpen(WebSocketChannel channel, Object context) {
    if (onOpen != null) {
      onOpen.invoke(RT.map(k_channel, channel,
                           k_context, context));
    }
  }
}
